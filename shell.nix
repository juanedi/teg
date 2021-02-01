let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
  ghc = import ./nix/ghc.nix;
in with nixpkgs;

pkgs.mkShell {
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
