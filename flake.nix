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
          esqueleto-textsearch = pkgs.haskell.lib.overrideCabal (hnew.callCabal2nix "esqueleto-textsearch" ./. { }) {
            postBuild = ''
                echo "entering the phase"
                ${pkgs.tree}/bin/tree
                mkdir -p $out/bin/test
                cp ./dist/build/spec/spec $out/bin/test/spec
            '';

            checkPhase = ''
            echo "ran by flake :)"
            '';
          }
            ;
        };
      };
      package = hpkgs.esqueleto-textsearch;
    in
    {
      defaultPackage.x86_64-linux = package;
      # to run this you need to
      # set the test suite to an executable in the cabal file
      checks.x86_64-linux.tests =  pkgs.nixosTest {
        name = "esqueleto-test";

        testScript = ''
          server.start()
          server.wait_for_unit("postgresql.service")
          print(
              server.succeed(
                  "${package}/bin/test/spec --fail-on-focused"
              )
          )
        '';
        nodes.server = {

          virtualisation.memorySize = 2048;
          virtualisation.diskSize = 1024;
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
