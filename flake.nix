{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    hell.url = "github:chrisdone/hell?ref=551133cecdafed1d6d3f4da7d8a466df2eed8af5";

  };
  outputs = inputs@{ self, nixpkgs, flake-parts, hell, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, system, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # If you have a .cabal file in the root, this option is determined
          # automatically. Otherwise, specify all your local packages here.
          # packages.example.root = ./.;

          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          # basePackages = pkgs.haskellPackages;

          # Dependency overrides go here. See https://haskell.flake.page/dependency
          # source-overrides = { };
          # overrides = self: super: with pkgs.haskell.lib; {
          # };

          devShell = {
            # Enabled by default
            enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {
              fourmolu = hp.fourmolu;
              hpack = hp.hpack;
              webook = pkgs.webhook;
              hell = hell.packages.${system}.default;

            };

          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.brossa-cd;
      };
    };
}
