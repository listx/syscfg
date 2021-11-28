{ profile ? "default" }:
with import <nixpkgs> { overlays = [ (import <rust-overlay>) ]; };
mkShell {
  nativeBuildInputs = [ rust-bin.stable.latest.${profile} ];
}
