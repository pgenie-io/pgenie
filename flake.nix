{
  description = "pgenie - Type-safe PostgreSQL client code generator";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  outputs = { self, nixpkgs, haskellNix, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ haskellNix.overlay ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc9123";

          # SHA-256 hashes for source-repository-package entries in cabal.project.
          # These are the hashes of the unpacked GitHub archives as computed by
          # nix-prefetch-url --unpack <url>.  If a hash is wrong, nix will print
          # the correct value in the error output.
          sha256map = {
            "https://github.com/nikita-volkov/hasql-dev"."76d8faaa706872e0fe9b1036aa011edd2eb9d0ed" =
              "sha256-sSxIsx63k/KoUKV2VMbj6hZ8Rv4adRz1C7Da0t2k+TM=";
            "https://github.com/nikita-volkov/syntactic-class"."b74373863e8bb0ef9877e0cd8dad5a3128d31818" =
              "sha256-Q6b5MP0jW3B37/TALiKpujksnoHXxGvOVhFa031BKFc=";
            "https://github.com/nikita-volkov/relations"."fac9e0445677a6493f1954f491921c5a7442cd58" =
              "sha256-QuaVZJhnYIqHo8dgBS83dsaM9ThJMI9XlSSR466SGsA=";
            "https://github.com/pgenie-io/gen-sdk"."v0.4.0" =
              "sha256-ABzzpi701lY+rq7TuYKTvu3T6YdwvDACjcrxNeGx9bQ=";
            "https://github.com/nikita-volkov/dhall-haskell"."c3264e5ed34566b66badb301211847ac4f78e697" =
              "sha256-2VQG7Xdoa3H+4XhqW7Flh78ZcOCOqJYqOwWHd/JwgIE=";
          };

          shell.tools = {
            cabal = { };
          };

          shell.buildInputs = with pkgs; [
            postgresql
          ];
        };

        flake = project.flake { };
      in
      flake
    );
}
