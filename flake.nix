{
  inputs = {
    cl-nix-lite.url = "github:hraban/cl-nix-lite";
  };

  outputs = { self, nixpkgs, flake-utils, cl-nix-lite }:
    flake-utils.lib.eachDefaultSystem (system:
      with rec {
        pkgs = nixpkgs.legacyPackages.${system}.extend cl-nix-lite.overlays.default;
      };
      {
        packages = {
          default = with pkgs.lispPackagesLite; lispScript {
            name = "plist-copy";
            src = ./main.lisp;
            dependencies = [
              alexandria
              inferior-shell
              str
              trivia
            ];
            nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
            postInstall = ''
              wrapProgramBinary "$out/bin/plist-copy" \
                --suffix PATH : "${pkgs.lib.makeBinPath [ pkgs.rsync pkgs.findutils ]}"
            '';
            installCheckPhase = ''
              $out/bin/plist-copy --help
            '';
            doInstallCheck = true;
          };
        };
      });
}
