{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
let
  salsa-party-production = import (./nixos-module.nix) { envname = "production"; salsaPartyPackages = pkgs.salsaPartyPackages; };
  port = 8001;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "salsa-party-module-test";
    nodes = {
      client = {
        imports = [
          salsa-party-production
        ];
        services.salsa-party.production = {
          enable = true;
          end-to-end-test = {
            enable = true;
            url = "machine:${builtins.toString port}";
          };
        };
      };
      server = {
        imports = [
          salsa-party-production
        ];
        services.salsa-party.production = {
          enable = true;
          web-server = {
            enable = true;
            inherit port;
          };
        };
      };
    };
    testScript = ''
      client.start()
      server.start()
      client.wait_for_unit("multi-user.target")
      server.wait_for_unit("multi-user.target")

      server.wait_for_open_port(${builtins.toString port})
      client.systemctl("start salsa-party-end-to-end-test-production.service --wait")
    '';
  }
)
