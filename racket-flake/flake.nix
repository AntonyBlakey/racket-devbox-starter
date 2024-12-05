{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = {
          racket-minimal = 
            pkgs.racket-minimal.overrideAttrs (finalAttrs: previousAttrs: {
                configureFlags = [
                    "--enable-${previousAttrs.shared}"
                    "--enable-lt=${pkgs.libtool}/bin/libtool"
                   "--enable-macprefix"
                ];
              });
          libX11 = pkgs.xorg.libX11;
        };
      });
}