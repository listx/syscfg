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
  nativeBuildInputs =
    [ rust-bin.stable.latest.${profile} pkg-config openssl ] ++ platform-specific;
}
