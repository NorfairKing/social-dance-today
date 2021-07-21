{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
let
  salsa-party-production = import (./nixos-module.nix) {
    envname = "production";
    inherit sources;
    salsaPartyPackages = pkgs.salsaPartyPackages;
  };
  port = 8001;
  testuser = "testuser";
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "salsa-party-selenium-test";
    nodes = {
      client = {
        imports = [
          salsa-party-production
        ];
        services.salsa-party.production = {
          enable = true;
          end-to-end-test = {
            enable = true;
            url = "http://server:${builtins.toString port}";
          };
        };

        users.users."${testuser}" = {
          isNormalUser = true;
        };

        systemd.services.selenium-test = {
          description = "selenium test";
          serviceConfig = {
            Type = "oneshot";
            User = testuser;
          };
          path = import ../selenium-tests/env.nix { inherit pkgs; };
          script = ''
            set -e
            mkdir -p /tmp/selenium-test
            cd /tmp/selenium-test
            python ${../selenium-tests/test.py} --headless --host server:${builtins.toString port}
          '';
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
            enable-osm-geocoding = false;
            enable-google-geocoding = false;
            events-info-importer.enable = false;
            golatindance-com-importer.enable = false;
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
      client.systemctl("start selenium-test.service --wait")
      client.systemctl("status selenium-test.service")
      client.require_unit_state("selenium-test.service", "inactive")
    '';
  }
)
