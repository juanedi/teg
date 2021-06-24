let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs-unstable { };
  niv = import sources.niv { };
  teg = import ./default.nix;
in with nixpkgs;

pkgs.mkShell {
  buildInputs = [
    niv.niv
    entr
    overmind

    ghcid
    cabal-install
    haskellPackages.ormolu

    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-live
  ];

  inputsFrom = [ teg.env ];
}
