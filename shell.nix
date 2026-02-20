{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:

let
  haskellDeps = ps: with ps; [
    postgresql-simple
    aeson
    text
    bytestring
    vector
    # hastorch  # Note: Keep this if you have it in your local overlay/pinned set
  ];
in
pkgs.mkShell {
  name = "hasxiom-dsl-env";

  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages haskellDeps)
    haskellPackages.cabal-install
    
    # System dependencies
    libtorch-bin
    cudatoolkit
    postgresql_16
    zlib          # Required for many Haskell networking/JSON libs
    jq            # Essential for MCP JSON orchestration
    git
  ];

  shellHook = ''
    echo "--- Hasxiom DSL: Solving the Coherency Gap ---"
    export LD_LIBRARY_PATH="${pkgs.libtorch-bin}/lib:${pkgs.cudatoolkit}/lib:${pkgs.zlib}/lib"
    
    # Identity Check
    export GIT_AUTHOR_NAME="Scott Baker"
    export GIT_AUTHOR_EMAIL="scott.bakerphx@gmail.com"
    
    export HASXIOM_DB_CONN="host=localhost dbname=hasxiom_db user=nixdude"
    
    echo "Sovereign environment active. jq and zlib loaded."
  '';
}
