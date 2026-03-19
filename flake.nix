{
  description = "pgenie - static build dependencies for CI";

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
        # Used only for the final cabal link step, NOT for building libpq itself.
        openssl-static = pkgs.openssl.override { static = true; };

        # libpq built against the regular dynamic OpenSSL so that PostgreSQL's
        # own macOS linker check passes.
        #
        # Background: PostgreSQL's build system runs a check
        # (src/interfaces/libpq/Makefile, target libpq-refs-stamp) that verifies
        # libpq.dylib does NOT reference _atexit or _pthread_exit.  When libpq is
        # compiled against a *static* OpenSSL, those symbols from the OpenSSL
        # static archive end up in libpq.dylib, causing the check to fail.
        # Building against dynamic OpenSSL avoids this because those symbols stay
        # inside libssl.dylib and are not embedded in libpq.dylib.  (This check
        # is macOS-specific but using the same build approach on Linux keeps the
        # two platforms consistent.)
        #
        # The libpq.a static archive contains only libpq's own compiled objects;
        # its OpenSSL symbol references are resolved at Cabal link time when we
        # supply the static OpenSSL archives via -optl flags.  This means the
        # compiled libpq.a is ABI-compatible with static or dynamic OpenSSL
        # (same version, same nixpkgs revision).
        #
        # Other notes:
        #   - gssSupport is disabled to avoid a dependency on Kerberos static
        #     libraries, which are not reliably available on CI runners.
        #     pgenie connects using password authentication, so GSSAPI / Kerberos
        #     is not required at runtime.
        #   - nixpkgs' default postInstall removes the static archives on
        #     non-static platforms; overriding it to a no-op keeps them in the
        #     $dev output so we can copy them into the static-only directory below.
        libpq-static = (pkgs.libpq.override {
          gssSupport = false;
        }).overrideAttrs (_: {
          postInstall = "";
        });

        # A directory containing ONLY static archives (.a files) and the libpq
        # headers needed by the postgresql-libpq Haskell package.
        #
        # Because there are no .dylib/.so files here, the linker cannot
        # silently resolve -lpq, -lssl, or -lcrypto to shared libraries when
        # cabal's extra-lib-dirs is pointed at this directory.
        static-deps = pkgs.runCommand "pgenie-static-deps" { } ''
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
        packages.macos-static-deps = static-deps;
        packages.linux-static-deps = static-deps;
        packages.default = static-deps;
      }
    );
}
