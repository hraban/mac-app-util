# Copyright © 2023–2024  Hraban Luyat
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, version 3 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

{
  inputs = {
    # This has SBCL 2.4.10 and docktuil 3.1.3 which are known to work
    nixpkgs.url = "github:NixOS/nixpkgs/af51545ec9a44eadf3fe3547610a5cdd882bc34e";
    cl-nix-lite.url = "github:hraban/cl-nix-lite";
    flake-compat = {
      # Use my own fixed-output-derivation branch because I don’t want users to
      # need to eval-time download dependencies.
      url = "github:hraban/flake-compat/fixed-output";
      flake = false;
    };
    systems.url = "github:nix-systems/default-darwin";
    flake-utils = {
      url = "flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs = { self, nixpkgs, flake-utils, cl-nix-lite, ... }:
    {
      homeManagerModules.default = { pkgs, lib, config, ... }: {
        options = with lib; {
          targets.darwin.mac-app-util.enable = mkOption {
            type = types.bool;
            default = builtins.hasAttr pkgs.stdenv.system self.packages;
            example = true;
            description = "Whether to enable mac-app-util home manager integration";
          };
        };
        config = lib.mkIf config.targets.darwin.mac-app-util.enable {
          home.activation = {
            trampolineApps = let
              mac-app-util = self.packages.${pkgs.stdenv.system}.default;
            in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
              fromDir="$HOME/Applications/Home Manager Apps"
              toDir="$HOME/Applications/Home Manager Trampolines"
              ${mac-app-util}/bin/mac-app-util sync-trampolines "$fromDir" "$toDir"
            '';
          };
        };
      };
      darwinModules.default = { pkgs, ... }: {
        system.activationScripts.postActivation.text = let
          mac-app-util = self.packages.${pkgs.stdenv.system}.default;
        in ''
          ${mac-app-util}/bin/mac-app-util sync-trampolines "/Applications/Nix Apps" "/Applications/Nix Trampolines"
        '';
      };
    }
    //
    (with flake-utils.lib; eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [ 
          (final: prev: {
            sbcl = prev.sbcl.overrideAttrs (prevPkg: rec {
              version = "2.5.2";
              src = prev.fetchurl {
                url = "mirror://sourceforge/project/sbcl/sbcl/${version}/sbcl-${version}-source.tar.bz2";
                sha256 = "sha256-XcJ+un3aQz31P9dEHeixFHSoLNrBaJwfbOVfoGXWX6w=";
              };
              patches = prevPkg.patches ++ [ ./sbcl.patch ];
            });
          })
          cl-nix-lite.overlays.default
        ];
      in {
        checks = {
          default = self.packages.${system}.default;
        };
        packages = {
          default = pkgs.callPackage ({
            lispPackagesLite
          , dockutil
          , findutils
          , jq
          , rsync
          }: with lispPackagesLite; lispScript rec {
            name = "mac-app-util";
            src = ./main.lisp;
            dependencies = [
              alexandria
              inferior-shell
              cl-interpol
              cl-json
              str
              trivia
            ];
            nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
            postInstall = ''
              wrapProgramBinary "$out/bin/${name}" \
                --suffix PATH : "${with pkgs; lib.makeBinPath [
                  dockutil
                  rsync
                  findutils
                  jq
                ]}"
            '';
            installCheckPhase = ''
              $out/bin/${name} --help
            '';
            doInstallCheck = true;
            meta.license = pkgs.lib.licenses.agpl3Only;
          }) {};
        };
      }));
}
