{ tag }:

let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { };
  ghc = (import ./nix/ghc.nix).release;
in
derivation {
  name = "teg";
  builder = "${nixpkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  system = builtins.currentSystem;
  inherit tag;
  inherit ghc;

  src = builtins.fetchurl "https://github.com/juanedi/teg/archive/${tag}.tar.gz";

  coreutils = nixpkgs.coreutils;
  gnutar = nixpkgs.gnutar;
  gzip = nixpkgs.gzip;

  elm = nixpkgs.elmPackages.elm;
  elm_format = nixpkgs.elmPackages.elm-format;
}
