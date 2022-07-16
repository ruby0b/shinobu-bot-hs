# shinobu-bot
This is a rewrite of https://github.com/ruby0b/shinobu-bot-py and will eventually supersede it.

## Deployment (using nix flakes)
1. Add the flake to your inputs:
```nix
  inputs.shinobu-bot-hs.url = "github:ruby0b/shinobu-bot-hs";
```
3. Use the overlay `inputs.shinobu-bot-hs.overlay.${system}`.
4. Import `inputs.shinobu-bot-hs.nixosModules.default`.
5. Enable the service in your configuration:
```nix
  services.shinobu-bot-hs = {
    enable = true;
    token = "MY_DISCORD_BOT_TOKEN";
  };
```

## Development
Start the hoogle webserver (http://localhost:8080/):
```sh
nix develop -c hoogle server >/dev/null 2>/dev/null &
```
