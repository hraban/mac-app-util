{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixcasks.url = "github:jacekszymanski/nixcasks";
  inputs.nixcasks.inputs.nixpkgs.follows = "nixpkgs";

  # inputs.mac-app-util.url = "github:hraban/mac-app-util";
  inputs.mac-app-util.url = "../.";

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , mac-app-util
    , nixcasks
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShells.default =
        let
          osVersion = "sonoma";

          nixcasksOverlay = final: prev: {
            nixcasks = (nixcasks.output {
              inherit osVersion;
            }).packages."${prev.system}";
          };

          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              nixcasksOverlay
            ];
          };

          brewPackages = [
            pkgs.nixcasks.android-studio
            pkgs.nixcasks.firefox
          ];
        in
        pkgs.mkShellNoCC {
          shellHook = ''
            # make apps available in the launcher and up to date in the dock
            ${pkgs.lib.getExe mac-app-util.packages."${system}".shellHook} \
              ${mac-app-util.lib.assembleBrewAppsInOneFolder { inherit pkgs brewPackages; }} \
              "Nix_Flake_Example"
          '';
        };
    });
}
