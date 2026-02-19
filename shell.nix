{ pkgs ? import <nixpkgs> {} }:

let
  haskellEnv = pkgs.haskellPackages.ghcWithPackages (p: with p; [
    postgresql-simple
    text
    aeson         # High-speed JSON parsing
    bytestring    # Required for AESON
  ]);
in
pkgs.mkShell {
  buildInputs = [
    haskellEnv
    pkgs.postgresql_16
    pkgs.jq       # This fixes the 'command not found' error
  ];

  shellHook = ''
    echo "--- Hasxiom Engineering Environment ---"
    echo "GHC: $(ghc --version)"
    echo "JQ:  $(jq --version)"
    echo "Postgres: $(psql --version)"
  '';
}
