{ salsa-party-nixos-module-factory
, nixosTest
}:
let
  salsa-party-production = salsa-party-nixos-module-factory {
    envname = "production";
  };
  port = 8047;
  ekg-port = 8048;
in
nixosTest (
  { lib, pkgs, ... }: {
    name = "salsa-party-module-test";
    nodes = {
      client = { };
      server = {
        imports = [
          salsa-party-production
        ];
        services.salsa-party.production = {
          enable = true;
          web-server = {
            enable = true;
            inherit port;
            inherit ekg-port;
            log-level = "Debug";
            enable-osm-geocoding = false;
            enable-google-geocoding = false;
            organiser-reminder.enable = false;
            importers.enable = true;
            importer = {
              "salsalovers.be" = {
                enable = false;
              };
            };
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
      client.succeed("curl server:${builtins.toString port}")
      server.wait_for_open_port(${builtins.toString ekg-port})
      client.succeed("curl server:${builtins.toString ekg-port}")
    '';
  }
)
