{
  description = "Peano numbers in Lean 4";

  inputs = { 
    nixpkgs.url = "github:NixOS/nixpkgs";

    lean.url = "github:leanprover/lean4";

    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, lean, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      leanPackages = lean.packages.${system};
      pkg = leanPackages.buildLeanPackage {
        name = "Peano";
        src = ./src;
      };
    in {
      packages = pkg // {
        inherit (leanPackages) lean;
      };

      defaultPackage = pkg.modRoot;
        devShell = pkgs.mkShell {
          buildInputs = [
            leanPackages.lean
          ];

          shellHook = ''
            export LEAN_PATH="$PWD/result"
            export LEAN_SRC_PATH="$PWD/src"
          '';
        };
    });
}
