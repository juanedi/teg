{ system ? builtins.currentSystem }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  teg = import ./teg.nix;

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
