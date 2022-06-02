{
  description = "A discord bot";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          modules = [
            ({ lib, ... }: {
              # https://github.com/input-output-hk/haskell.nix/issues/829
              config.dontStrip = false;
              # https://github.com/input-output-hk/haskell.nix/issues/1177
              config.reinstallableLibGhc = true;
            })
          ];
          overlays = [
            haskellNix.overlay
            # why is this required even though sqlite3 is aliased in haskell.nix?
            (final: prev: { sqlite3 = final.sqlite; })
            (final: prev: {
              shinobu-bot = final.haskell-nix.project' rec {
                src = ./.;
                compiler-nix-name = "ghc923";
                inherit modules;
                shell = {
                  tools = {
                    cabal = { inherit modules; };
                    haskell-language-server = {
                      inherit modules;
                      # adapted from https://github.com/haskell/haskell-language-server/blob/140f9040ae88352ca1140a750e7c26485fdfbe17/cabal.project
                      cabalProject = ''
                        packages: ./

                        tests: true

                        package *
                          test-show-details: direct

                        write-ghc-environment-files: never

                        constraints:
                          hyphenation +embed,
                          hlint +ghc-lib,
                          ghc-lib-parser-ex -auto,
                          stylish-haskell +ghc-lib,
                          haskell-language-server -haddockComments,

                        allow-newer:
                          -- ghc-9.2
                          ----------
                          hiedb:base,
                          retrie:ghc-exactprint,

                          -- for brittany
                          -- https://github.com/lspitzner/multistate/pull/8
                          multistate:base,
                          -- https://github.com/lspitzner/data-tree-print/pull/3
                          data-tree-print:base,
                          -- https://github.com/lspitzner/butcher/pull/8
                          butcher:base,

                          -- for shake-bench
                          Chart:lens,
                          Chart-diagrams:lens,

                          -- for ekg
                          ekg-core:base,
                          ekg-core:ghc-prim,
                          ekg-wai:base,
                          ekg-wai:time,

                          -- for shake-bench
                          Chart-diagrams:diagrams-core,
                          SVGFonts:diagrams-core
                      '';
                    };
                  };
                  buildInputs = with pkgs; [
                    nixpkgs-fmt
                    haskellPackages.cabal-fmt
                  ];
                };
              };
            })
          ];
          pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          flake = pkgs.shinobu-bot.flake { };
          exeName = "shinobu-bot:exe:shinobu-bot";
        in
        flake // rec {
          packages = flake.packages // {
            shinobu-bot-hs = flake.packages.${exeName};
            default = flake.packages.${exeName};
          };
          apps = flake.apps // { default = flake.apps.${exeName}; };
          overlay = (_: _: packages);
        }
      ) // {
      nixosModules.default = { pkgs, lib, config, ... }:
        let
          inherit (lib) mkOption types mkIf;
          name = "shinobu-bot-hs";
          cfg = config.services.${name};
        in
        {
          options.services.${name} = {
            enable = mkOption { type = types.bool; default = false; };
            stateDir = mkOption { type = types.str; default = name; };
            token = mkOption { type = types.uniq types.str; };
          };
          config = mkIf cfg.enable {
            systemd.services.${name} = {
              description = "Shinobu - a Discord bot written in Haskell";
              wantedBy = [ "multi-user.target" ];
              after = [ "network.target" ];

              serviceConfig = {
                ExecStart = "${pkgs.shinobu-bot-hs}/bin/shinobu-bot";
                WorkingDirectory = "/var/lib/${cfg.stateDir}";
                StateDirectory = cfg.stateDir;
                Restart = "always";
                RestartSec = 20;
                DynamicUser = true;
              };

              preStart = ''
                echo '${cfg.token}' >TOKEN
              '';
            };
          };
        };
    };
}
