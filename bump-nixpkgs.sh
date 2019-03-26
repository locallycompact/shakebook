#!/usr/bin/env fish
set -l a (curl https://api.github.com/repos/NixOS/nixpkgs/git/refs/heads/master | jq -r '.object.sha')
set -l b (nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/$a.tar.gz)
jq -n --arg rev "$a" --arg sha "$b" '{ owner: "NixOS", repo: "nixpkgs", rev: $rev, sha256: $sha }' > nixpkgs-src.json
