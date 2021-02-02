let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { };
  baseDeps = ps:
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
  developmentDeps = ps:
    with ps; [
      hspec
      hspec-wai
      hspec-wai-json
    ];
in with nixpkgs.haskellPackages;
{
  development = ghcWithPackages ( ps : (baseDeps ps) ++ (developmentDeps ps));
  release = ghcWithPackages baseDeps;
}
