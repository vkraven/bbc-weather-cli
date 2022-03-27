{ pkgs ? import <nixpkgs> {} }:

let ghc = pkgs.haskellPackages.ghcWithPackages (hPkgs: [
      hPkgs.http-client
      hPkgs.http-client-tls
      hPkgs.http-types
      hPkgs.transformers
      hPkgs.split
      hPkgs.aeson
      hPkgs.aeson-pretty
      hPkgs.cmdargs
      hPkgs.uri-encode

      # hPkgs.haskell-language-server
    ]);
in
pkgs.mkShell {
      buildInputs = [ ghc 
                      # pkgs.haskell-language-server
                      pkgs.git 
                      pkgs.cabal-install
                      pkgs.cabal2nix ];
}
