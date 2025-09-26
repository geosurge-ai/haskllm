{
  description = "Blood Money is an inference platform for chunkier LLMs.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    passveil.url = "github:doma-engineering/passveil";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      passveil,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };
        lib = pkgs.lib;
        hsPkgs = pkgs.haskellPackages;

        gcloud = pkgs.google-cloud-sdk.withExtraComponents (
          with pkgs.google-cloud-sdk.components;
          [
            alpha
            beta
          ]
        );
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs =
            with pkgs;
            [
              # Rust toolchain
              rustc
              cargo
              rustfmt
              clippy
              rust-analyzer

              # TypeScript toolchain
              nodejs
              pnpm
              typescript

              # Haskell toolchain (for Literate Haskell)
              hsPkgs.ghc
              hsPkgs.cabal-install
              hsPkgs.haskell-language-server
              hsPkgs.hlint
              hsPkgs.ghcid
              hsPkgs.fourmolu
              hsPkgs.hoogle
              hsPkgs.cabal-gild
              pandoc
              poppler_utils
              lhs2tex
              (texlive.combine { inherit (texlive) scheme-medium polytable lazylist; })

              # Terraform
              terraform
              terraform-ls

              # Google Cloud SDK with alpha and beta components
              gcloud

              # Essential tools
              pkg-config
              openssl.dev
              openssl
              curl
              git
              jq
              watch
              tree
              dnsutils

              # Docker for container workflows
              docker
              docker-compose

              # Script development tools
              shellcheck
              shfmt

              # Golang for cloud compute
              go

              # Act for CI/CD
              act
              # act -W .github/workflows/extension-ci.yaml --container-architecture linux/arm64 -P ubuntu-latest
            ]
            ++ lib.optionals pkgs.stdenv.isLinux [
              xorg.xorgserver
              # Browser testing tools (needed for e2e tests)
              chromium
              chromedriver
            ];

          # On macOS expose Home-brew's binaries
          shellHook = lib.optionalString pkgs.stdenv.isLinux ''
            export CHROME_BIN=${pkgs.chromium}/bin/chromium
            export CHROMEDRIVER_BIN=${pkgs.chromedriver}/bin/chromedriver
            export PATH=$(dirname "$CHROMEDRIVER_BIN"):$PATH
            export SELENIUM_MANAGER_SKIP_DOWNLOAD=1
          '';

          # rust-analyzer needs the std source tree
          RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
        };
      }
    );
}
