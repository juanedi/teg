{ system ? builtins.currentSystem, tag }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  teg = import ./teg.nix { inherit tag; };

  name = "jedi/teg";
  tag = "latest";

in pkgs.dockerTools.buildLayeredImage {
  inherit name tag;
  contents = [ teg ];

  config = {
    Cmd = [ "/teg/bin/teg" "--server" ];
    WorkingDir = "/";
  };
}
