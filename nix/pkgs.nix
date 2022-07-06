{ sources ? import ./sources.nix
}:
let
  pkgsf = import sources.nixpkgs;
in
pkgsf {
  overlays =
    [
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
      (import (sources.autodocodec + "/nix/overlay.nix"))
      (import (sources.ical + "/nix/overlay.nix"))
      (import (sources.linkcheck + "/nix/overlay.nix"))
      (import (sources.looper + "/nix/overlay.nix"))
      (import (sources.pretty-relative-time + "/nix/overlay.nix"))
      (import (sources.safe-coloured-text + "/nix/overlay.nix"))
      (import (sources.seocheck + "/nix/overlay.nix"))
      (import (sources.sydtest + "/nix/overlay.nix"))
      (import (sources.typed-uuid + "/nix/overlay.nix"))
      (import (sources.validity + "/nix/overlay.nix"))
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
