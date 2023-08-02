# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          esqueleto-textsearch = hnew.callCabal2nix "esqueleto-textsearch" ./. { };
        };
      };
    in
    {
      defaultPackage.x86_64-linux = pkgs.haskell.lib.dontCheck hpkgs.esqueleto-textsearch;
      # to run this you need to
      # set the test suite to an executable in the cabal file
      checks.x86_64-linux.tests =  pkgs.nixosTest {
        name = "esqueleto-test";

        testScript = ''
          server.start()
          server.wait_for_unit("postgresql.service")
          print(
              server.succeed(
                  "${hpkgs.esqueleto-textsearch}/bin/spec --fail-on-focused"
              )
          )
        '';
        nodes.server = {
          services.postgresql = {
            enable = true;
            initialScript = pkgs.writeText "psql-init" ''
              CREATE USER test WITH SUPERUSER PASSWORD 'test';
              CREATE DATABASE test WITH OWNER test;
            '';
          };
        };
      };
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."esqueleto-textsearch" ];
        withHoogle = true;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
        ];
      };
    };
}
