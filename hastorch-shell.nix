{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:

pkgs.mkShell {
  name = "hastorch-env";
  
  # Tenet: Pinning our engineering tools
  buildInputs = with pkgs; [
    haskell.compiler.ghc96
    cabal-install
    pkg-config
    zlib
    libtorch-bin
    cudaPackages.cudatoolkit
    cudaPackages.cudnn
    postgresql
    jq
    git # Needed for the cabal.project git fetch
  ];

  # This allows libtorch-ffi to find the C++ headers and libraries
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.libtorch-bin}/lib:${pkgs.cudaPackages.cudatoolkit}/lib:${pkgs.stdenv.cc.cc.lib}/lib:$LD_LIBRARY_PATH"
    export CUDA_PATH="${pkgs.cudaPackages.cudatoolkit}"
    export HASKELL_TORCH_CUDA=1
    
    # Check if cabal.project exists, create it if missing to resolve libtorch-ffi
    if [ ! -f cabal.project ]; then
      echo ">> Creating cabal.project for Sovereign Linkage..."
      echo "packages: ." > cabal.project
      echo "" >> cabal.project
      echo "source-repository-package" >> cabal.project
      echo "    type: git" >> cabal.project
      echo "    location: https://github.com/hasktorch/hasktorch" >> cabal.project
      echo "    tag: master" >> cabal.project
      echo "    subdir:" >> cabal.project
      echo "        hasktorch" >> cabal.project
      echo "        libtorch-ffi" >> cabal.project
      echo "        libtorch-ffi-helper" >> cabal.project
    fi

    echo "================================================"
    echo ">> HASXIOM SOVEREIGN BRAIN ENVIRONMENT ACTIVE <<"
    echo ">> GPU: NVIDIA Ultra 7 Accelerated            <<"
    echo ">> Purity: Proprietary Hastorch Engine        <<"
    echo "================================================"
  '';
}
