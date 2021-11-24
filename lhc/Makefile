all:
	nix-shell -p pkg-config openssl --command "cargo install --path . --debug"
test:
	nix-shell -p pkg-config openssl --command "cargo test"
