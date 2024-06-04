# {
#   # This is a template created by `hix init`
#   inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
#   inputs.flake-utils.url = "github:numtide/flake-utils";
#
#   outputs = { self, nixpkgs, flake-utils, haskellNix }:
#     let
#       supportedSystems =
#         in
#           flake-utils.lib.eachSystem
#           supportedSystems
#           (system:
#             let
#               overlays = [
#                 haskellNix.overlay
#                 (final: prev: {
#                   hixProject =
#                     final.haskell-nix.hix.project {
#                       src = ./.;
#                       evalSystem = "x86_64-linux";
#                     };
#                 })
#               ];
#               pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
#               flake = pkgs.hixProject.flake { };
#             in
#             flake // {
#               legacyPackages = pkgs;
#               packages.flake = flake;
#               packages.default = flake.packages."cabal-plan-to-nix:exe:cabal-plan-to-nix";
#             });
#
#       # --- Flake Local Nix Configuration ----------------------------
#       nixConfig = {
#         # This sets the flake to use the IOG nix cache.
#         # Nix should ask for permission before using it,
#         # but remove it here if you do not want it to.
#         extra-substituters = [ "https://cache.iog.io" ];
#         extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
#         allow-import-from-derivation = "true";
#       };
#       }
{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = inputs@{ nixpkgs, flake-parts, haskell-nix, ... }:
    flake-parts.lib.mkFlake
      { inherit inputs; }
      {
        systems =
          [
            "x86_64-linux"
            "x86_64-darwin"
            "aarch64-linux"
            "aarch64-darwin"
          ];
        perSystem = { self', system, pkgs, ... }:
          let
            project = pkgs.haskell-nix.cabalProject' {
              compiler-nix-name = "ghc96";
              src = ./.;
              shell.tools = {
                cabal = "latest";
                cabal-plan = "latest";
                fourmolu = "0.14.0.0";
              };
            };

            flake = project.flake (
              pkgs.lib.attrsets.optionalAttrs
                (system == "x86_64-linux")
                { crossPlatforms = p: [ p.musl64 ]; }
            );
          in
          {
            _module.args.pkgs = haskell-nix.legacyPackages.${system};

            inherit (flake) apps checks devShells;
            packages = flake.packages // rec {
              cabal-plan-to-nix = flake.packages."cabal-plan-to-nix:exe:cabal-plan-to-nix";
              default = cabal-plan-to-nix;
            };
          };
      };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}
