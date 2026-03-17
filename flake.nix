{
  description = "pgenie - macOS static build dependencies for CI";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # OpenSSL built with static archives (.a files) included.
        openssl-static = pkgs.openssl.override { static = true; };

        # libpq built with static archives retained.
        #
        # Notes:
        #   - gssSupport is disabled to avoid a dependency on Kerberos static
        #     libraries, which are not reliably available on macOS CI runners.
        #     pgenie connects using password authentication, so GSSAPI / Kerberos
        #     is not required at runtime.
        #   - PostgreSQL's build system always builds both shared and static
        #     libraries.  nixpkgs' default postInstall removes the static
        #     archives on non-static platforms; overriding it to a no-op keeps
        #     them in the $dev output so we can copy them into the static-only
        #     library directory below.
        libpq-static = (pkgs.libpq.override {
          gssSupport = false;
          openssl = openssl-static;
        }).overrideAttrs (_: {
          postInstall = "";
        });

        # A directory containing ONLY static archives (.a files) and the libpq
        # headers needed by the postgresql-libpq Haskell package.
        #
        # Because there are no .dylib files here, the macOS linker cannot
        # silently resolve -lpq, -lssl, or -lcrypto to shared libraries when
        # cabal's extra-lib-dirs is pointed at this directory.
        macos-static-deps = pkgs.runCommand "pgenie-macos-static-deps" { } ''
          set -euo pipefail
          mkdir -p $out/lib $out/include

          # Static archives.
          # Note: OpenSSL has multiple outputs; libraries reside in the 'out'
          # output (not the default 'bin' output), so we reference .out explicitly.
          cp ${libpq-static.dev}/lib/libpq.a       $out/lib/
          cp ${openssl-static.out}/lib/libssl.a    $out/lib/
          cp ${openssl-static.out}/lib/libcrypto.a $out/lib/

          # Headers required by the postgresql-libpq Haskell binding
          cp -r ${libpq-static.dev}/include/*      $out/include/
        '';

      in
      {
        packages.macos-static-deps = macos-static-deps;
        packages.default = macos-static-deps;
      }
    );
}
