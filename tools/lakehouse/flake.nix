{
  description = "Nix Engineering Lakehouse - Full Stack";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      
      haskellEnv = pkgs.haskellPackages.ghcWithPackages (p: [
        p.postgresql-simple
        p.text
        p.aeson
        p.bytestring
        p.containers
      ]);

      nix-tree-pkg = pkgs.stdenv.mkDerivation {
        name = "nix-tree";
        src = ./.;
        buildInputs = [ haskellEnv ];
        buildPhase = "ghc -O2 Tree.hs -o nix-tree";
        installPhase = ''
          mkdir -p $out/bin
          cp nix-tree $out/bin/
        '';
      };
    in
    {
      packages.${system}.default = nix-tree-pkg;

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [ 
          haskellEnv
          pkgs.postgresql_16
          pkgs.jq
          nix-tree-pkg 
        ];

        shellHook = ''
          echo "--- Nix Engineering Lakehouse (Flake Mode: Full Stack) ---"
          echo "Modules: postgresql-simple, aeson, bytestring, containers"
        '';
      };
    };
}
