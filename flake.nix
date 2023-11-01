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
            name = "app-plist-copy";
            src = ./main.lisp;
            dependencies = [
              alexandria
              inferior-shell
              trivia
            ];
          };
        };
      });
}
