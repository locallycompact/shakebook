{ pkgs ? import ./nixpkgs.nix {}, ... }:
let
   f = import (./stack2nix.nix) { pkgs = pkgs; };
   henv = f.ghcWithPackages (p: with p; [
      data-default
      diagrams
      dihaa
      inline-r
      pandoc
      rio
      shake
      slick
   ]);
in
with pkgs;
stdenv.mkDerivation {
  name = "shakebook-shell";
  buildInputs = [ henv R texlive.combined.scheme-small gnome3.librsvg];
}
