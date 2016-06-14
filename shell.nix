{
    nixpkgs ? import <nixpkgs> {},
    compiler ? "ghc7103"
}:
let
  ghc = nixpkgs.pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
      bytestring wai http-types warp time postgresql-simple safe wai-extra aeson string-conversions http-conduit blaze-from-html blaze-html basic-prelude wai-middleware-static
    ]);
in
    nixpkgs.pkgs.stdenv.mkDerivation {
        name = "my-haskell-env-0";
        buildInputs = [ ghc ];
        shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
    }
