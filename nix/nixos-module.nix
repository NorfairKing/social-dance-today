{ envname
, sources ? import ./sources.nix
, salsaPartyPackages ? (import ./pkgs.nix { }).salsaPartyPackages
}:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.salsa-party."${envname}";

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

  toYamlFile = pkgs.callPackage ./to-yaml.nix { };

  mkLooperOption = pkgs.callPackage (sources.looper + "/nix/looper-option.nix") { };
in
{
  options.services.salsa-party."${envname}" =
    {
      enable = mkEnableOption "Salsa/Party Service";
      web-server =
        mkOption {
          type =
            types.nullOr (types.submodule {
              options =
                {
                  enable = mkEnableOption "Salsa/Party WEB Server";
                  config =
                    mkOption {
                      description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
                      default = { };
                    };
                  log-level = mkOption {
                    type = types.str;
                    example = "LevelDebug";
                    default = "LevelWarn";
                    description = "The log level to use";
                  };
                  hosts = mkOption {
                    type = types.listOf (types.str);
                    example = "salsa-party.cs-syd.eu";
                    default = [ ];
                    description = "The host to serve web requests on";
                  };
                  port = mkOption {
                    type = types.int;
                    example = 8001;
                    description = "The port to serve web requests on";
                  };
                  admin = mkOption {
                    type = types.nullOr types.str;
                    example = "syd@cs-syd.eu";
                    default = null;
                    description = "The email address of the administrator";
                  };
                  send-emails = mkOption {
                    type = types.nullOr types.bool;
                    example = true;
                    default = null;
                    description = "Whether to require email verifications and send emails";
                  };
                  send-address = mkOption {
                    type = types.nullOr types.str;
                    example = "no-reply@social-dance.today";
                    default = null;
                    description = "The email address to send emails from";
                  };
                  enable-osm-geocoding = mkOption {
                    type = types.nullOr types.bool;
                    example = true;
                    default = null;
                    description = "Whether to enable OpenStreetMaps-based geocoding";
                  };
                  enable-google-geocoding = mkOption {
                    type = types.nullOr types.bool;
                    example = true;
                    default = null;
                    description = "Whether to enable Google-based geocoding";
                  };
                  google-api-key = mkOption {
                    type = types.nullOr types.str;
                    example = "XXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXX";
                    default = null;
                    description = "The google API key";
                  };
                  google-analytics-tracking = mkOption {
                    type = types.nullOr types.str;
                    example = "X-XXXXXXXXXX";
                    default = null;
                    description = "The google Analytics tracking code";
                  };
                  google-search-console-verification = mkOption {
                    type = types.nullOr types.str;
                    example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXX";
                    default = null;
                    description = "The google Search Console verification code";
                  };
                  sentry =
                    mkOption {
                      default = null;
                      type =
                        types.nullOr (types.submodule {
                          options =
                            {
                              dsn = mkOption {
                                type = types.str;
                                example = "https://XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX@XXXXXXX.ingest.sentry.io/XXXXXXX";
                                description = "The Sentry Data Source Name";
                              };
                              release = mkOption {
                                type = types.str;
                                example = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
                                description = "The Sentry Release";
                              };
                            };
                        });
                    };
                  image-garbage-collector = mkLooperOption "image-garbage-collector";
                  organiser-reminder = mkLooperOption "organiser-reminder";
                  events-info-importer = mkLooperOption "events-info-importer";
                  golatindance-com-importer = mkLooperOption "golatindance-com-importer";
                  danceplace-com-importer = mkLooperOption "danceplace-com-importer";
                  mapdance-com-importer = mkLooperOption "mapdance-com-importer";
                  salsachicago-com-importer = mkLooperOption "salsachicago-com-importer";
                };
            });
          default = null;
        };
      end-to-end-test = mkOption {
        default = null;
        type = types.nullOr (types.submodule {
          options = {
            # We have only one of these because we don't need any backward/forward compatibilty, only current compatibility.
            enable = mkEnableOption "End-to-end tests for the ${envname} environment of the salsa party web server";
            url = mkOption {
              type = types.str;
              example = "https://staging.salsa-parties.today";
              description = "The url to the server to test";
            };
            time =
              mkOption {
                type = types.str;
                example = "03:00";
                default = "03:00";
                description = "The time of day to start the end-to-end test.";
              };
          };
        });
      };
    };
  config =
    let
      nullOrOption =
        name: opt: optionalAttrs (!builtins.isNull opt) { "${name}" = opt; };
      nullOrOptionHead =
        name: opt: optionalAttrs (!builtins.isNull opt && opt != [ ]) { "${name}" = builtins.head opt; };

      web-server-config = with cfg.web-server;
        mergeListRecursively [
          (nullOrOption "log-level" log-level)
          (nullOrOptionHead "host" hosts)
          (nullOrOption "port" port)
          (nullOrOption "admin" admin)
          (nullOrOption "send-emails" send-emails)
          (nullOrOption "send-address" send-address)
          (nullOrOption "enable-osm-geocoding" enable-osm-geocoding)
          (nullOrOption "enable-google-geocoding" enable-google-geocoding)
          (nullOrOption "google-api-key" google-api-key)
          (nullOrOption "google-analytics-tracking" google-analytics-tracking)
          (nullOrOption "google-search-console-verification" google-search-console-verification)
          (nullOrOption "image-garbage-collector" image-garbage-collector)
          (nullOrOption "organiser-reminder" organiser-reminder)
          (nullOrOption "events-info-importer" events-info-importer)
          (nullOrOption "golatindance-com-importer" golatindance-com-importer)
          (nullOrOption "danceplace-com-importer" danceplace-com-importer)
          (nullOrOption "mapdance-com-importer" mapdance-com-importer)
          (nullOrOption "salsachicago-com-importer" salsachicago-com-importer)
          (nullOrOption "sentry" sentry)
          cfg.web-server.config
        ];
      web-server-config-file = toYamlFile "salsa-web-server-config" web-server-config;

      working-dir = "/www/salsa-party/${envname}/";
      # The docs server
      web-server-working-dir = working-dir + "web-server/";
      # The api server
      web-server-service =
        optionalAttrs (cfg.web-server.enable or false) {
          "salsa-party-web-server-${envname}" = {
            description = "Salsa Party web Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "SALSA_PARTY_WEB_SERVER_CONFIG_FILE" =
                  "${web-server-config-file}";
              };
            script =
              ''
                mkdir -p "${web-server-working-dir}"
                cd ${web-server-working-dir};
                ${salsaPartyPackages.salsa-party-web-server}/bin/salsa-party-web-server
              '';
            serviceConfig =
              {
                Restart = "always";
                RestartSec = 1;
                Nice = 15;
              };
            unitConfig =
              {
                StartLimitIntervalSec = 0;
                # ensure Restart=always is always honoured
              };
          };
        };
      web-server-host =
        with cfg.web-server;

        let redirectHost = host: {
          "www.${host}" = {
            enableACME = true;
            forceSSL = true;
            globalRedirect = host;
          };
        };
        in
        optionalAttrs (enable && hosts != [ ]) ({
          "${head hosts}" =
            {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://localhost:${builtins.toString port}";
              };
              serverAliases = tail hosts;
            };
        } // mergeListRecursively (builtins.map redirectHost hosts));
      end-to-end-test-service =
        optionalAttrs (cfg.end-to-end-test.enable or false) {
          "salsa-party-end-to-end-test-${envname}" = {
            description = "Salsa Party End to end tests for ${envname} Service";
            after = [ "salsa-party-web-server-${envname}" ];
            environment =
              {
                "SALSA_PARTY_SERVER_URL" = "${cfg.end-to-end-test.url}";
              };
            script =
              ''
                ${salsaPartyPackages.salsa-party-web-server}/bin/salsa-party-web-server-e2e
              '';
            serviceConfig =
              {
                Type = "oneshot";
              };
          };
        };
      end-to-end-test-timer =
        optionalAttrs (cfg.end-to-end-test.enable or false) {
          "salsa-party-end-to-end-test-${envname}" = {
            description = "Salsa Party End to end tests for ${envname} Service";
            wantedBy = [ "timers.target" ];
            timerConfig = {
              OnCalendar = "*-*-* ${cfg.end-to-end-test.time}";
            };
          };
        };
    in
    mkIf cfg.enable {
      systemd.services =
        mergeListRecursively [
          web-server-service
          end-to-end-test-service
        ];
      systemd.timers =
        mergeListRecursively [
          end-to-end-test-timer
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional (cfg.web-server.enable or false) cfg.web-server.port)
      ];
      services.nginx.virtualHosts =
        mergeListRecursively [
          web-server-host
        ];
    };
}
