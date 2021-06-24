let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs-unstable { };
in
nixpkgs.haskellPackages.callCabal2nix "teg" ./. { }
