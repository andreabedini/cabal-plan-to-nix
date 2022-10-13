{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Distribution.Client.Config (getCabalDir)
import Distribution.Client.DistDirLayout
  ( CabalDirLayout,
    defaultDistDirLayout,
    mkCabalDirLayout,
  )
import Distribution.Client.GlobalFlags
import Distribution.Client.HashValue (HashValue, showHashValue)
import Distribution.Client.HttpUtils (configureTransport)
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.PackageHash
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
  ( rebuildInstallPlan,
    rebuildProjectConfig,
  )
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Setup
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.Repo
import Distribution.Client.Types.SourceRepo
import Distribution.Compat.Directory (makeAbsolute)
import Distribution.PackageDescription qualified as PD
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Command
import Distribution.Simple.Flag
import Distribution.Types.PackageId
import Distribution.Verbosity (moreVerbose)
import Distribution.Verbosity qualified as Verbosity
import Network.URI (URI (uriPath))
import Nix
import System.Environment (getArgs)
import System.FilePath

main :: IO ()
main = do
  args <- getArgs
  case commandParseArgs cmdUI True args of
    CommandHelp help -> putStrLn (help "something")
    CommandList opts -> putStrLn $ "commandList" ++ show opts
    CommandErrors errs -> putStrLn $ "commandErrors: " ++ show errs
    CommandReadyToGo (mkflags, _commandParse) ->
      let globalFlags = defaultGlobalFlags
          flags@NixStyleFlags {configFlags} = mkflags (commandDefaultFlags cmdUI)
          verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
          cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty
       in installPlanAction verbosity cliConfig

cmdUI :: CommandUI (NixStyleFlags ())
cmdUI =
  CommandUI
    { commandName = "",
      commandSynopsis = "Makes an install-plan",
      commandUsage = ("Usage: " ++),
      commandDescription = Nothing,
      commandNotes = Nothing,
      commandDefaultFlags = defaultNixStyleFlags (),
      commandOptions = nixStyleOptions (const [])
    }

installPlanAction :: Verbosity.Verbosity -> ProjectConfig -> IO ()
installPlanAction verbosity cliConfig = do
  let ProjectConfigShared
        { projectConfigDistDir,
          projectConfigProjectFile
        } = projectConfigShared cliConfig
  let mProjectFile = flagToMaybe projectConfigProjectFile
  let mDistDirectory = flagToMaybe projectConfigDistDir

  Right projectRoot <- findProjectRoot Nothing mProjectFile

  let distDirLayout = defaultDistDirLayout projectRoot mDistDirectory

  httpTransport <- configureTransport verbosity mempty mempty

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      -- more verbose here to list the project files which have affected
      -- the project configuration with no extra options
      (moreVerbose verbosity)
      httpTransport
      distDirLayout
      cliConfig

  cabalDirLayout <- cabalDirLayoutFromProjectConfig projectConfig

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (_improvedPlan, elaboratedPlan, _elaboratedSharedConfig, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  print $ prettyNix $ makeNixPlan elaboratedPlan

cabalDirLayoutFromProjectConfig :: ProjectConfig -> IO CabalDirLayout
cabalDirLayoutFromProjectConfig
  ProjectConfig
    { projectConfigBuildOnly =
        ProjectConfigBuildOnly
          { projectConfigLogsDir
          },
      projectConfigShared =
        ProjectConfigShared
          { projectConfigStoreDir
          }
    } = do
    cabalDir <- getCabalDir
    mstoreDir <- traverse makeAbsolute $ flagToMaybe projectConfigStoreDir

    return $ mkCabalDirLayout cabalDir mstoreDir (flagToMaybe projectConfigLogsDir)

makeNixPlan :: ElaboratedInstallPlan -> NExpr
makeNixPlan elaboratedInstallPlan =
  "srp" ==> "self" ==> mkNonRecSet pkgs
  where
    pkgs = [elaboratedPackageToNix ecp | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedInstallPlan]

    elaboratedPackageToNix :: ElaboratedConfiguredPackage -> Binding NExpr
    elaboratedPackageToNix
      ElaboratedConfiguredPackage
        { elabUnitId,
          elabPkgSourceId = PackageIdentifier {pkgName, pkgVersion},
          elabFlagAssignment,
          elabPkgSourceLocation,
          elabPkgSourceHash,
          elabPkgDescriptionOverride
        } =
        T.pack (quoted $ prettyShow elabUnitId)
          $= mkNonRecSet
            ( [ "pkg-name" $= mkStr (T.pack $ prettyShow pkgName),
                "pkg-version" $= mkStr (T.pack $ prettyShow pkgVersion),
                "flags" $= flagAssignmentToNix elabFlagAssignment,
                "pkg-src" $= packageLocationToNix elabPkgSourceLocation elabPkgSourceHash
              ]
                ++ toList
                  ( fmap
                      ( \bs ->
                          "pkg-cabal-file" $= mkIndentedStr 4 (T.decodeUtf8 $ BL.toStrict bs)
                      )
                      elabPkgDescriptionOverride
                  )
            )

packageLocationToNix :: PackageLocation (Maybe FilePath) -> Maybe PackageSourceHash -> NExpr
packageLocationToNix (RepoTarballPackage (RepoLocalNoIndex _lr _s) _pkgId _local) (Just _srcHash) =
  error "not implemented"
packageLocationToNix (RepoTarballPackage RepoRemote {repoRemote} pkgId _local) (Just srcHash) =
  fetchRemoteRepoPackage repoRemote pkgId srcHash
packageLocationToNix (RepoTarballPackage RepoSecure {repoRemote} pkgId _local) (Just srcHash) =
  fetchRemoteRepoPackage repoRemote pkgId srcHash
packageLocationToNix (LocalUnpackedPackage local) _ =
  mkPath False local
packageLocationToNix (LocalTarballPackage local) _ =
  mkPath False local
packageLocationToNix (RemoteTarballPackage uri _local) (Just srcHash) =
  fetch uri srcHash
packageLocationToNix (RemoteSourceRepoPackage srcRepo _) (Just srcHash) =
  case srcRepo of
    SourceRepositoryPackage repoType srpLocation srpTag srpBranch srpSubdir _ ->
      "srp"
        @@ mkNonRecSet
          ( catMaybes
              [ Just $ "type" $= mkStr (T.pack $ prettyShow repoType),
                Just $ "location" $= mkStr (T.pack srpLocation),
                Just $ "sdist-sha256" $= mkStr (T.pack $ showHashValue srcHash),
                (\b -> "branch" $= mkStr (T.pack b)) <$> srpBranch,
                (\t -> "tag" $= mkStr (T.pack t)) <$> srpTag,
                (\s -> "subdir" $= mkStr (T.pack s)) <$> srpSubdir
              ]
          )
packageLocationToNix _ Nothing =
  error "No source hash available"

fetchRemoteRepoPackage :: RemoteRepo -> PackageId -> PackageSourceHash -> NExpr
fetchRemoteRepoPackage RemoteRepo {remoteRepoURI} pkgId =
  fetch uri
  where
    uri =
      remoteRepoURI
        { uriPath = uriPath remoteRepoURI </> "package" </> prettyShow pkgId </> prettyShow pkgId <.> ".tar.gz"
        }

fetch :: URI -> HashValue -> NExpr
fetch uri hash =
  "builtins" @. "fetchurl"
    @@ mkNonRecSet
      [ "url" $= mkStr (T.pack $ show uri),
        "sha256" $= mkStr (T.pack $ showHashValue hash)
      ]

flagAssignmentToNix :: PD.FlagAssignment -> NExpr
flagAssignmentToNix flagAssignment =
  mkNonRecSet [T.pack (prettyShow fn) $= mkBool fv | (fn, fv) <- PD.unFlagAssignment flagAssignment]

quoted :: (IsString a, Semigroup a) => a -> a
quoted str = "\"" <> str <> "\""
