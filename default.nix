{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./np.nix {}
