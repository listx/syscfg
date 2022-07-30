# See
# https://elixirforum.com/t/could-not-compile-file-system-watcher-for-mac/17432/10
# for a discussion about getting a file system watcher working on Mac.

{ profile ? "default" }:
with import <nixpkgs> { overlays = [ (import <rust-overlay>) ]; };
let
  # See available packages for darwin.apple_sdk.frameworks with
  #
  #  $ nix-env -f '<nixpkgs>' -qaPA darwin.apple_sdk.frameworks
  platform-specific = if stdenv.hostPlatform.isDarwin then
    (with darwin.apple_sdk.frameworks; [
      Security
    ]) else [];
in mkShell {
  RUST_SRC_PATH = "${rust-bin.stable.latest.${profile}.override {
              extensions = [ "rust-src" ];
          }}/lib/rustlib/src/rust/library";
  nativeBuildInputs =
    [ rust-bin.stable.latest.${profile} pkg-config openssl ] ++ platform-specific;
  buildInputs = [
    #beam.packages.erlangR25.elixir_1_13
    #nodejs-14_x
    (rust-bin.stable.latest.${profile}.override {
      extensions = ["rust-src"];
    })
  ]
  ++ lib.optionals stdenv.isLinux [
    # For ExUnit Notifier on Linux.
    libnotify

    # For file_system on Linux.
    inotify-tools
  ]
  ++ lib.optionals stdenv.isDarwin ([
    # For ExUnit Notifier on macOS.
    terminal-notifier

    # For file_system on macOS.
    darwin.apple_sdk.frameworks.CoreFoundation
    darwin.apple_sdk.frameworks.CoreServices
  ]);
}
