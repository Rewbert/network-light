# Sanity check that binaries built with either compiler behave like
# normal Unix filters: read all of stdin (via `interact`), flush, and
# exit cleanly enough for a downstream pipe stage to see EOF. Doesn't
# touch System.Network at all -- this is about the toolchain, not the
# library -- but a regression here (e.g. lazy IO or buffering behaving
# differently under mhs) would be confusing to debug from inside a
# socket test, so it gets its own.
#
# Usage:
#   make test              # build and run with mhs (default)
#   make test HC=ghc       # build and run with GHC
#   make clean

.PHONY: all test clean

HC ?= mhs

all: test

ifeq ($(HC),mhs)
upper: Upper.hs
	mhs Upper -o upper
countlines: CountLines.hs
	mhs CountLines -o countlines
else ifeq ($(HC),ghc)
upper: Upper.hs
	ghc -outputdir dist-ghc -main-is Upper Upper.hs -o upper
countlines: CountLines.hs
	ghc -outputdir dist-ghc -main-is CountLines CountLines.hs -o countlines
else
upper countlines:
	$(error Unknown HC '$(HC)', expected 'mhs' or 'ghc')
endif

test: upper countlines
	@got=$$(printf "banana\napple\ncherry\ndate\n" | sort | ./upper | grep A | ./countlines); \
	if [ "$$got" = "3" ]; then \
		echo "PASS: bash-pipes ($(HC))"; \
	else \
		echo "FAIL: bash-pipes ($(HC)): expected 3, got '$$got'"; \
		exit 1; \
	fi

clean:
	rm -rf upper countlines dist-ghc *.hi *.o
