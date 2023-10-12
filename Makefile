SHELL   =  /bin/bash

TS_REPO ?= https://github.com/nvim-treesitter/tree-sitter-query
TSDIR   =  $(notdir $(TS_REPO))
TESTDIR ?= .tests/nvim-treesitter/queries
TEST    ?= query/highlights.query

all:
	@

dev: $(TSDIR) ## download and build tree-sitter parser
$(TSDIR):
	@git clone --depth=1 $(TS_REPO)
	@printf "\e[1m\e[31mNote\e[22m\e[0m npm build can take a while\n" >&2
	cd $(TSDIR) &&                                         \
		npm --loglevel=info --progress=true install && \
		npx tree-sitter generate &&                    \
		./bin/download-examples.sh

.PHONY: parse
parse:
	cd $(TSDIR) && npx tree-sitter parse $(TESTDIR)/$(TEST)

clean:
	$(RM) *~
distclean: clean
	$(RM) -rf $$(git ls-files --others --ignored --exclude-standard)
