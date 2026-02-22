{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:

let
  haskellDeps = ps: with ps; [
    postgresql-simple
    aeson
    text
    bytestring
    vector
    haskeline
  ];

  # Sovereignty: We wrap cabal run so the tool feels like a native binary
  hasxiom-bin = pkgs.writeShellScriptBin "hasxiom" ''
    exec cabal run -v0 identify-pillars -- "$@"
  '';
in
pkgs.mkShell {
  name = "hasxiom-dsl-env";

  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages haskellDeps)
    haskellPackages.cabal-install
    hasxiom-bin
    
    # Tooling
    graphviz        # Critical for 'dot' command
    postgresql_16
    jq
    git
    
    # Compute Foundation
    libtorch-bin
    cudatoolkit
    zlib
  ];

  shellHook = ''
    echo "--- Hasxiom DSL: Solving the Coherency Gap ---"
    export LD_LIBRARY_PATH="${pkgs.libtorch-bin}/lib:${pkgs.cudatoolkit}/lib:${pkgs.zlib}/lib"
    
    export GIT_AUTHOR_NAME="Scott Baker"
    export GIT_AUTHOR_EMAIL="scott.bakerphx@gmail.com"
    
    # Tenet: Check connectivity to nixlakehouse
    export HASXIOM_DB_CONN="host=localhost dbname=hasxiom_db user=nixdude"
    
    echo "Sovereign environment active. Just type 'hasxiom' to enter the REPL."
    echo "GraphViz 'dot' command is now available for galaxy visualization."
  '';
}
