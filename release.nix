{ system ? builtins.currentSystem, imageName, tag }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  teg = import ./teg.nix { inherit tag; };

in pkgs.dockerTools.buildLayeredImage {
  name = imageName;
  inherit tag;
  contents = [ teg ];

  config = {
    Cmd = [ "/bin/teg" "--server" ];
    WorkingDir = "/";
  };
}
