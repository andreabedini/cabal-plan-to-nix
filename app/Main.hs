{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy qualified as BL
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
import Distribution.Client.ProjectPlanning (rebuildInstallPlan, rebuildProjectConfig)
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Setup hiding (cabalVersion)
import Distribution.Client.Types.ConfiguredId
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.Repo
import Distribution.Client.Types.SourceRepo
import Distribution.Client.Version (cabalInstallVersion)
import Distribution.Compat.Directory (makeAbsolute)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Package (HasUnitId (installedUnitId), installedDepends, packageId)
import Distribution.PackageDescription
import Distribution.Pretty (Pretty, prettyShow)
import Distribution.Simple.Command
import Distribution.Simple.Compiler (showCompilerId)
import Distribution.Simple.Flag
import Distribution.Simple.Utils
import Distribution.System
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
  (_improvedPlan, elaboratedPlan, elaboratedShared, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  print $ prettyNix $ makeNixPlan elaboratedPlan elaboratedShared

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

makeNixPlan :: ElaboratedInstallPlan -> ElaboratedSharedConfig -> NExpr
makeNixPlan elaboratedInstallPlan elaboratedShared =
  "srp"
    ==> "self"
    ==> mkNonRecSet
      [ "cabal-install-version" $= mkStr (display cabalInstallVersion),
        "cabal-version" $= mkStr (display cabalVersion),
        "compiler-id" $= mkStr (T.pack $ showCompilerId $ pkgConfigCompiler elaboratedShared),
        "os" $= mkStr (display os),
        "arch" $= mkStr (display arch),
        "install-plan" $= installPlanToNix (InstallPlan.toList elaboratedInstallPlan)
      ]
  where
    (Platform arch os) = pkgConfigPlatform elaboratedShared

    installPlanToNix :: [ElaboratedPlanPackage] -> NExpr
    installPlanToNix pkgs = mkNonRecSet [displayQuoted (installedUnitId pkg) $= go pkg | pkg <- pkgs]
      where
        go pkg = case pkg of
          InstallPlan.PreExisting ipi -> installedPackageInfoToNix ipi
          InstallPlan.Configured elab -> elaboratedPackageToNix False elab
          InstallPlan.Installed elab -> elaboratedPackageToNix True elab

    installedPackageInfoToNix :: InstalledPackageInfo -> NExpr
    installedPackageInfoToNix ipi =
      mkNonRecSet $
        catMaybes
          [ Just $ "type" $= mkStr "pre-existing",
            Just $ "id" $= mkStr (display $ installedUnitId ipi),
            Just $ "pkg-id" $= mkStr (display $ packageId ipi),
            case installedDepends ipi of
              [] -> Nothing
              libdeps -> Just $ "depends" $= mkList ["self" @. displayQuoted uid | uid <- libdeps]
          ]

    elaboratedPackageToNix :: Bool -> ElaboratedConfiguredPackage -> NExpr
    elaboratedPackageToNix isInstalled elab =
      mkNonRecSet $
        catMaybes
          [ Just $ "type" $= mkStr (if isInstalled then "installed" else "configured"),
            Just $ "pkg-id" $= mkStr (display $ packageId elab),
            Just $ "pkg-src" $= packageLocationToNix (elabPkgSourceLocation elab) (elabPkgSourceHash elab),
            case flagAssignmentToNix (elabFlagAssignment elab) of
              fs | fs == emptySet -> Nothing
              fs -> Just ("flags" $= fs),
            case elabLibDependencies elab of
              [] -> Nothing
              libdeps -> Just $ "lib-depends" $= mkList ["self" @. displayQuoted (confInstId cid) | cid <- libdeps],
            case elabExeDependencies elab of
              [] -> Nothing
              exedeps -> Just $ "exe-depends" $= mkList ["self" @. displayQuoted cid | cid <- exedeps],
            case elabSetupDependencies elab of
              [] -> Nothing
              setupdeps -> Just $ "setup-depends" $= mkList ["self" @. displayQuoted (confInstId cid) | cid <- setupdeps],
            case elabPkgConfigDependencies elab of
              [] -> Nothing
              pkgConfigDeps ->
                Just $ "pkg-condig-depends" $= mkList [pkgconfigDependsToNix n mv | (n, mv) <- pkgConfigDeps],
            (\pdo -> "pkg-cabal-file" $= mkIndentedStr 4 (T.decodeUtf8 $ BL.toStrict pdo)) <$> elabPkgDescriptionOverride elab
          ]

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
              [ Just $ "type" $= mkStr (display repoType),
                Just $ "location" $= mkStr (T.pack srpLocation),
                Just $ "sdist-sha256" $= mkStr (T.pack $ showHashValue srcHash),
                (\b -> "branch" $= mkStr (T.pack b)) <$> srpBranch,
                (\t -> "tag" $= mkStr (T.pack t)) <$> srpTag,
                (\s -> "subdir" $= mkStr (T.pack s)) <$> srpSubdir
              ]
          )
packageLocationToNix _ Nothing =
  error "No source hash available"

pkgconfigDependsToNix :: PkgconfigName -> Maybe PkgconfigVersion -> NExpr
pkgconfigDependsToNix n mv =
  mkNonRecSet $
    catMaybes
      [ Just $ "name" $= mkStr (displayQuoted n),
        fmap (\v -> "version" $= mkStr (display v)) mv
      ]

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

flagAssignmentToNix :: FlagAssignment -> NExpr
flagAssignmentToNix flagAssignment =
  mkNonRecSet [display fn $= mkBool fv | (fn, fv) <- unFlagAssignment flagAssignment]

quoted :: (IsString a, Semigroup a) => a -> a
quoted str = "\"" <> str <> "\""

display :: Pretty a => a -> T.Text
display = T.pack . prettyShow

displayQuoted :: Pretty a => a -> T.Text
displayQuoted = T.pack . quoted . prettyShow
