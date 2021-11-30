{ profile ? "default" }:
with import <nixpkgs> { overlays = [ (import <rust-overlay>) ]; };
let
  platform-specific = lib.optionalString stdenv.hostPlatform.isDarwin
    "darwin.apple_sdk.frameworks.Security";
in mkShell {
  nativeBuildInputs =
    [ rust-bin.stable.latest.${profile} pkg-config openssl platform-specific ];
}
