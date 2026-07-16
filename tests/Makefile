# Runs every test under unittests/ with both compilers, so a change can
# be checked against cabal/ghc and mcabal/mhs in one shot.
#
# Usage:
#   make test              # run every test under both mhs and ghc
#   make test HC=mhs       # run every test under mhs only
#   make test HC=ghc       # run every test under ghc only
#   make clean             # clean every test directory

.PHONY: all test clean

UNITTESTS := $(sort $(patsubst %/,%,$(dir $(wildcard unittests/*/Makefile))))

ifeq ($(HC),)
HCS := mhs ghc
else
HCS := $(HC)
endif

all: test

test:
	@fail=0; \
	total=0; \
	for d in $(UNITTESTS); do \
		for hc in $(HCS); do \
			total=$$((total+1)); \
			$(MAKE) -s -C $$d clean >/dev/null 2>&1; \
			if ! $(MAKE) -s -C $$d test HC=$$hc; then fail=$$((fail+1)); fi; \
			$(MAKE) -s -C $$d clean >/dev/null 2>&1; \
		done; \
	done; \
	echo "-----"; \
	if [ $$fail -eq 0 ]; then \
		echo "all $$total test runs passed"; \
	else \
		echo "$$fail of $$total test runs FAILED"; \
	fi; \
	exit $$fail

clean:
	@for d in $(UNITTESTS); do $(MAKE) -s -C $$d clean; done
