{
  description = "A discord bot";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ]
    (system:
      let
        overlays = [
          inputs.haskellNix.overlay
          # why is this required even though sqlite3 is aliased in haskell.nix?
          (final: prev: { sqlite3 = final.sqlite; })
          (final: prev: {
            shinobu-bot = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc928";
              shell = {
                tools = {
                  cabal = { };
                };
                buildInputs = with pkgs; [
                  nixpkgs-fmt
                  haskellPackages.cabal-fmt
                ];
              };
            };
          })
        ];
        pkgs = import inputs.nixpkgs { inherit system overlays; inherit (inputs.haskellNix) config; };
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
              RestartSec = 5;
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
