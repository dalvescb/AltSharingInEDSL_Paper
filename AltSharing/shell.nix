{ nixpkgs ? import (builtins.fetchTarball {
  name = "nixpkgs-unstable-2022-11-27";
  url = "https://github.com/NixOS/nixpkgs/archive/f82f0ec1b70b2879c3f3d9a1015a05c73a90a17c.tar.gz";
  sha256 = "0kpn948x2i25wwc3gvbm9s8qd470yspxbz2wkwgnxs9p4l6zv4ci";
  }) {} }:

let
in nixpkgs.haskell.lib.buildStackProject {
  name = "AltSharing";
  nativeBuildInputs = [ nixpkgs.pkg-config ];
  buildInputs = [
      nixpkgs.haskell.compiler.ghc924
		  nixpkgs.ghcid
		  nixpkgs.cabal-install
		  nixpkgs.zlib
		  nixpkgs.graphviz
      nixpkgs.haskellPackages.hp2pretty
		  nixpkgs.haskellPackages.ormolu
		  nixpkgs.haskellPackages.hoogle
		  nixpkgs.haskellPackages.zlib
		  nixpkgs.haskellPackages.hlint
      nixpkgs.haskellPackages.mtl
      nixpkgs.haskellPackages.fgl
      nixpkgs.haskellPackages.unordered-containers
		  (nixpkgs.haskell-language-server.override { supportedGhcVersions = [ "924" ]; })
		  nixpkgs.gnumake
		  nixpkgs.coreutils
		  nixpkgs.hdf5
		  nixpkgs.fftw
		  nixpkgs.gcc
		];
}

  # To use this add the following to the bottom of your stack.yaml
  # nix:
  #   enable: true
  #   shell-file: shell.nix

  # also execute the following in the project root
  # echo "use nix" >> .envrc
  # direnv allow
