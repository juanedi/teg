{ tag }:

let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  haskellDeps = ps:
    with ps; [
      base
      aeson
      async
      blaze-markup
      blaze-html
      bytestring
      casing
      conduit
      containers
      directory
      elm-bridge
      fast-logger
      filepath
      http-api-data
      process
      servant
      servant-blaze
      servant-server
      servant-websockets
      stm
      text
      uuid
      wai
      wai-app-static
      wai-logger
      warp
      websockets

      # for tests
      hspec
      hspec-wai
      hspec-wai-json
    ];
  ghc = nixpkgs.haskellPackages.ghcWithPackages haskellDeps;
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
