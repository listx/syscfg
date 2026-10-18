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

vendored := \
	vendor/source-fonts \
	vendor/mathjax \
	vendor/mathjax-newcm-font

vendor/source-fonts:
	./vendor-source-fonts.sh

vendor/mathjax vendor/mathjax-newcm-font &:
	./vendor-mathjax.sh

site.tar.gz: $(archive_file) weave $(vendored)
	rm -rf site.tar.gz site/vendor
	cp -r vendor site
	tar czvf site.tar.gz -C site .

PAGES_BRANCH := gh-pages
PAGES_DOMAIN := syscfg.funloop.org
PAGES_REMOTE ?= $(shell git remote get-url $$(git config branch.main.remote) || git remote get-url origin)
PAGES_GIT_NAME  ?= $(or $(shell git config user.name),pages-publisher)
PAGES_GIT_EMAIL ?= $(or $(shell git config user.email),pages-publisher@localhost)

publish: $(archive_file) weave $(vendored)
	rm -rf site/vendor
	cp -r vendor site
	touch site/.nojekyll
	echo $(PAGES_DOMAIN) > site/CNAME
	cd site && \
		git add -A && \
		{ git diff --cached --quiet || \
		  git -c user.name="$(PAGES_GIT_NAME)" -c user.email="$(PAGES_GIT_EMAIL)" \
			commit -q -m "update"; } && \
		git push -f "$(PAGES_REMOTE)" $(PAGES_BRANCH)
	touch publish

lint: $(archive_file)
	cd .. && lilac --project-conf doc/Lilac.toml lint doc/$<
	prek run --all-files
	touch lint

gitconfig:
	cd .. && git config diff.orderfile .git-orderfile
.PHONY: gitconfig

update:
	cd .. && niv update nixpkgs --branch nixos-26.05
.PHONY: update

clean:
	rm -rf \
		$(archive_file) \
		$(obj_files)
.PHONY: clean
