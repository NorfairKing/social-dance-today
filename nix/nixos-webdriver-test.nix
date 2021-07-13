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


        # Turn on the x server.
        # What follows here is taken from
        # https://github.com/NixOS/nixpkgs/blob/8284fc30c84ea47e63209d1a892aca1dfcd6bdf3/nixos/tests/common/auto.nix#L42-L66
        services.xserver.enable = true;
        # Use IceWM as the window manager.
        # Don't use a desktop manager.
        services.xserver.displayManager = {
          defaultSession = lib.mkDefault "none+icewm";
          lightdm.enable = true;
          autoLogin = {
            enable = true;
            user = testuser;
          };
        };
        services.xserver.windowManager.icewm.enable = true;
        # lightdm by default doesn't allow auto login for root, which is
        # required by some nixos tests. Override it here.
        security.pam.services.lightdm-autologin.text = lib.mkForce ''
          auth     requisite pam_nologin.so
          auth     required  pam_succeed_if.so quiet
          auth     required  pam_permit.so
          account  include   lightdm
          password include   lightdm
          session  include   lightdm
        '';

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
      client.wait_for_x()
      client.systemctl("start webdriver-test.service --wait")
      client.systemctl("status webdriver-test.service")
      client.require_unit_state("webdriver-test.service", "inactive")
    '';
  }
)
