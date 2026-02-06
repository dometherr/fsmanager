{
    description = "FS Manager's Dev Environment";

    inputs.nixpkgs.url     = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    inputs.flake-utils.url = "github:numtide/flake-utils";

    outputs =
    {
        self,
        nixpkgs,
        flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (
        system:
        let 
            pkgs = import nixpkgs { inherit system; };

            compiler    = "ghc912";
            haskellPkgs = pkgs.haskell.packages.${compiler};

            ghc = haskellPkgs.ghcWithPackages (pkg: [
                pkg.zlib
                pkg.lzma
            ]);
            hfmt  = haskellPkgs.stylish-haskell;
            hlsrv = haskellPkgs.haskell-language-server; 
            hlint = haskellPkgs.hlint;
            ghcid = haskellPkgs.ghcid;
        in
        {
            devShells.default = pkgs.mkShell {
                buildInputs = [
                    ghc
                    ghcid
                    hfmt
                    hlsrv
                    hlint

                    pkgs.cabal-install
                    pkgs.stack
                ];

                shellHook = ''
                    echo "Welcome to FS Manager Environment"
                    echo "=========================================================================="
                    echo "   GHC: $(ghc --version)"
                    echo "   Cabal: $(cabal --version | head -n 1)"
                    echo "   Stack: $(stack --version)"
                    echo "========================================================================="
                '';
            };
        }
    );
}
