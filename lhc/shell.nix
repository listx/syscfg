{ profile ? "default" }:
with import <nixpkgs> { overlays = [ (import <rust-overlay>) ]; };
let
  platform-specific =
    if stdenv.isDarwin then darwin.apple_sdk.frameworks.Security else { };
in mkShell {
  nativeBuildInputs =
    [ rust-bin.stable.latest.${profile} pkg-config openssl platform-specific ];
}
