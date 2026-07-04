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
	lilac weave $< --out-dir site --write-css --write-js
	touch weave

site.tar.gz: $(archive_file) weave vendor/mathjax vendor/source-fonts
	rm -f site.tar site.tar.gz
	tar cvf site.tar -C site .
	tar rvf site.tar vendor
	gzip site.tar

SRHT_TOKEN != cat .srht-pages-token 2>/dev/null || echo ""

publish: weave
	@if [ -z "$(SRHT_TOKEN)" ]; then \
		echo "Error: .srht-pages-token is empty or missing." >&2; \
		exit 1; \
	fi
	@echo "Uploading site to SourceHut Pages..."
	@curl --oauth2-bearer "$(SRHT_TOKEN)" \
		-Fcontent=@site.tar.gz \
		https://pages.sr.ht/publish/syscfg.funloop.org
	touch publish

lint: $(archive_file)
	cd .. && lilac --project-conf doc/Lilac.toml lint doc/$<
	prek run --all-files
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
		$(obj_files)
.PHONY: clean
