{ profile ? "default" }:
with import <nixpkgs> { overlays = [ (import ~/.nix-defexpr/channels/rust-overlay) ]; };
let
  platform-specific = if stdenv.hostPlatform.isDarwin then
    (with darwin.apple_sdk.frameworks; [
      Security
    ]) else [];
in mkShell {
  RUST_SRC_PATH = "${rust-bin.stable.latest.${profile}.override {
              extensions = [ "rust-src" ];
          }}/lib/rustlib/src/rust/library";
  nativeBuildInputs =
    [ rust-bin.stable.latest.${profile} pkg-config openssl ];
  buildInputs = [
    (rust-bin.stable.latest.${profile}.override {
      extensions = ["rust-src"];
    })
  ];
}
