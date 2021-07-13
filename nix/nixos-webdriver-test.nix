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
    name = "salsa-party-webdriver-test";
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

        systemd.services.webdriver-test = {
          description = "webdriver test";
          serviceConfig = {
            Type = "oneshot";
            User = testuser;
          };
          path =
            let
              my-python-packages = python-packages: with python-packages; [
                selenium
              ];
              python-with-my-packages = pkgs.python3.withPackages my-python-packages;
            in
            [
              pkgs.geckodriver
              pkgs.firefox
              python-with-my-packages
            ];
          script = ''
            set -e
            mkdir -p /tmp/webdriver-test
            cd /tmp/webdriver-test
            python ${../webdriver-tests/test.py}
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
            dance-us-org-importer.enable = false;
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
      client.systemctl("start webdriver-test.service --wait")
      client.systemctl("status webdriver-test.service")
      client.require_unit_state("webdriver-test.service", "inactive")
    '';
  }
)
