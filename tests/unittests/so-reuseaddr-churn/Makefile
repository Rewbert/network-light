# Rebinds the same port many times in a row, each time via a fresh
# socket, right after actively closing an accepted connection on it.
# Without SO_REUSEADDR this fails with EADDRINUSE once TIME_WAIT sockets
# pile up; with it, it should sail through every iteration.
#
# Deliberately drives the connections from plain bash (/dev/tcp) rather
# than another Haskell process, so this test doesn't depend on
# concurrency working inside the compiled binary (see the note in
# concurrent-clients/ and send-string/'s README about forkIO deadlocking
# blocking FFI calls under GHC).
#
# Usage:
#   make test              # build and run with mhs (default)
#   make test HC=ghc       # build and run with GHC, against the library source
#   make clean

.PHONY: all test clean

SHELL  := /bin/bash
HC     ?= mhs
TOPSRC := ../../../src
PORT   := 9933
ITERS  := 20

all: test

ifeq ($(HC),mhs)
server: Server.hs
	mhs -i$(TOPSRC) Server -o server
else ifeq ($(HC),ghc)
server: Server.hs
	ghc -outputdir dist-ghc -i$(TOPSRC) -main-is Server Server.hs -o server
else
server:
	$(error Unknown HC '$(HC)', expected 'mhs' or 'ghc')
endif

test: server
	@rm -f server.out
	@( timeout 20 ./server $(ITERS) > server.out 2>&1 & )
	@for i in $$(seq 1 $(ITERS)); do \
		j=0; \
		while [ $$j -lt 50 ] && ! grep -q "READY_$$i$$" server.out 2>/dev/null; do \
			sleep 0.05; j=$$((j+1)); \
		done; \
		if ! grep -q "READY_$$i$$" server.out 2>/dev/null; then \
			echo "FAIL: so-reuseaddr-churn ($(HC)): server never reached iteration $$i"; \
			cat server.out; exit 1; \
		fi; \
		bash -c 'exec 3<>/dev/tcp/127.0.0.1/$(PORT); sleep 0.2' 2>/dev/null || { \
			echo "FAIL: so-reuseaddr-churn ($(HC)): could not connect on iteration $$i"; \
			cat server.out; exit 1; \
		}; \
	done
	@sleep 0.3
	@if grep -q SERVER_DONE server.out; then \
		echo "PASS: so-reuseaddr-churn ($(HC))"; \
	else \
		echo "FAIL: so-reuseaddr-churn ($(HC)): server did not complete all iterations"; \
		cat server.out; exit 1; \
	fi

clean:
	rm -rf server dist-ghc *.hi *.o server.out
