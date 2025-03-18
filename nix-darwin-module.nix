{ mac-app-util-packages }:
{ pkgs, config, lib, ... }: with lib; {
  options = {
    mac-app-util = {
      enable = mkOption {
        description = "Whether to enable mac-app-util nix-darwin integration";
        type = types.bool;
        default = true;
      };
      hide-trampolines-folder = mkOption {
        description = "Hide the folder containing the trampolines in `/Applications/`";
        type = types.bool;
        default = false;
      };
    };
  };

  config = let
    cfg = config.mac-app-util;
  in
    mkIf cfg.enable {
      system.activationScripts.postActivation.text = let
        mac-app-util = mac-app-util-packages.${pkgs.stdenv.system}.default;
      in ''
        trampolines_dir="/Applications/Nix Trampolines"
        nix_apps_dir="/Applications/Nix Apps"

        echo "mac-app-util: syncing trampolines for $nix_apps_dir..."
        ${mac-app-util}/bin/mac-app-util sync-trampolines "$nix_apps_dir" "$trampolines_dir"
      ''
      + strings.optionalString cfg.hide-trampolines-folder ''
        chflags hidden "$trampolines_dir"
      '';
    };
}
