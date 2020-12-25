let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
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
    ];
  ghc = nixpkgs.haskellPackages.ghcWithPackages haskellDeps;
in with nixpkgs;
stdenv.mkDerivation {
  name = "teg";
  buildInputs = [
    niv.niv
    entr
    overmind

    ghc
    haskellPackages.ormolu

    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-live
  ];

}