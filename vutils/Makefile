tag          := $(shell git describe --tags --abbrev=0)
commit_count := $(shell git rev-list --count $(tag)..@)

all: test
.PHONY: all doc clean build check test

doc:
	Rscript -e "devtools::document()"

clean:
	rm -rf vutils_*.tar.gz vutils.Rcheck/

build: clean doc
	R CMD build .
	@mv vutils_*.tar.gz vutils_$(tag)-$(commit_count).tar.gz

check: build
	R CMD check vutils_*.tar.gz

test:
	Rscript -e "devtools::test(reporter = SummaryReporter)"
