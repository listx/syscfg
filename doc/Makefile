org_files := $(shell find . -type f -name "*.org")

obj_files := $(org_files:%.org=%.lobj)

archive_file := syscfg.larc

all: tangle weave lint
.PHONY: all

$(obj_files) &: $(org_files)
	lilac compile $?
	touch $(obj_files)

$(archive_file): $(obj_files)
	lilac archive $^ --out-file $@

tangle: $(archive_file)
	lilac tangle $< --out-dir ..
	touch tangle

weave: $(archive_file)
	lilac weave $< --out-dir ../site --root-relative-prefix "/site" --write-css --write-js
	touch weave

lint: $(archive_file)
	cd .. && lilac lint doc/$<
	#cd .. && editorconfig-checker
	touch lint

gitconfig:
	cd .. && git config diff.orderfile .git-orderfile
.PHONY: gitconfig

update:
	cd .. && niv update nixpkgs --branch nixos-25.11
.PHONY: update

clean:
	rm -rf \
		$(archive_file) \
		$(obj_files) \
		../site/*
.PHONY: clean
