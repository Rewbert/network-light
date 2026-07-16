# Checks the two failure-reporting conventions the library offers for a
# connect that can't succeed: `connect` throws, `connect'` returns False.
#
# Usage:
#   make test              # build and run with mhs (default)
#   make test HC=ghc       # build and run with GHC, against the library source
#   make clean

.PHONY: all test clean

HC     ?= mhs
TOPSRC := ../../../src

all: test

ifeq ($(HC),mhs)
throws: ConnectThrows.hs
	mhs -i$(TOPSRC) ConnectThrows -o throws
safe: ConnectSafe.hs
	mhs -i$(TOPSRC) ConnectSafe -o safe
else ifeq ($(HC),ghc)
throws: ConnectThrows.hs
	ghc -outputdir dist-ghc -i$(TOPSRC) -main-is ConnectThrows ConnectThrows.hs -o throws
safe: ConnectSafe.hs
	ghc -outputdir dist-ghc -i$(TOPSRC) -main-is ConnectSafe ConnectSafe.hs -o safe
else
throws safe:
	$(error Unknown HC '$(HC)', expected 'mhs' or 'ghc')
endif

test: throws safe
	@./throws > throws.out 2>&1; rc=$$?; \
	if [ $$rc -eq 0 ]; then \
		echo "FAIL: connect-closed-port ($(HC)): connect to a closed port did not fail"; \
		cat throws.out; exit 1; \
	fi
	@./safe > safe.out 2>&1; rc=$$?; \
	if [ $$rc -ne 0 ] || ! grep -q "CONNECT_RESULT:False" safe.out; then \
		echo "FAIL: connect-closed-port ($(HC)): connect' did not cleanly return False"; \
		cat safe.out; exit 1; \
	fi
	@echo "PASS: connect-closed-port ($(HC))"

clean:
	rm -rf throws safe dist-ghc *.hi *.o *.out
