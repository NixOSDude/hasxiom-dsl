{
  description = "Hasxiom DSL: Sovereign AI & Engineering Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      # Load the pillars safely
      pinnedPillars = if builtins.pathExists ./pinned-packages.nix 
                      then import ./pinned-packages.nix { inherit pkgs; }
                      else [];

      haskellDeps = ps: with ps; [
        postgresql-simple
        aeson
        text
        bytestring
        vector
        mcp-server
        containers
      ];

      # Define the helper script as a proper package
      pillarsScript = pkgs.writeShellScriptBin "hasxiom-pillars" ''
        if [ -f ./pinned-packages.nix ]; then
          cat ./pinned-packages.nix
        else
          echo "Error: pinned-packages.nix not found in current directory."
        fi
      '';

    in {
      devShells.${system}.default = pkgs.mkShell {
        name = "hasxiom-dsl-env";

        buildInputs = with pkgs; [
          (haskellPackages.ghcWithPackages haskellDeps)
          haskellPackages.cabal-install
          libtorch-bin
          cudatoolkit
          linuxPackages.nvidia_x11
          postgresql_16
          ollama-cuda
          graphviz
          pillarsScript # Explicitly adding the binary here
        ];

        shellHook = ''
          echo "--- Hasxiom DSL: Sovereign AI Grid Active ---"
          export LD_LIBRARY_PATH="${pkgs.libtorch-bin}/lib:${pkgs.cudatoolkit}/lib:${pkgs.linuxPackages.nvidia_x11}/lib"
          export TORCH_CUDA=1
          export GIT_AUTHOR_NAME="Scott Baker"
          export GIT_AUTHOR_EMAIL="scott.bakerphx@gmail.com"
          
          echo ">> [Sovereignty] ${toString (builtins.length pinnedPillars)} core pillars identified."
          echo "RTX 3060 CUDA paths and 'hasxiom-pillars' command solidified."
        '';
      };
    };
}
