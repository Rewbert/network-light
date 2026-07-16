# Checks that a network-light client can sit inside a normal bash
# pipeline: stdin is piped in from a real shell command, the client
# forwards that over a socket to a network-light server, the server's
# reply comes back out the client's stdout, and a further bash stage
# consumes it. Exercises the library's sockets together with mhs/GHC's
# ordinary stdin/stdout handling in the same process, not just the
# toolchain in isolation.
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
server: Server.hs
	mhs -i$(TOPSRC) Server -o server
client: Client.hs
	mhs -i$(TOPSRC) Client -o client
else ifeq ($(HC),ghc)
server: Server.hs
	ghc -outputdir dist-ghc -i$(TOPSRC) -main-is Server Server.hs -o server
client: Client.hs
	ghc -outputdir dist-ghc -i$(TOPSRC) -main-is Client Client.hs -o client
else
server client:
	$(error Unknown HC '$(HC)', expected 'mhs' or 'ghc')
endif

test: server client
	@rm -f server.out
	@( timeout 10 ./server > server.out 2>&1 & )
	@i=0; \
	while [ $$i -lt 50 ] && ! grep -q READY server.out 2>/dev/null; do sleep 0.1; i=$$((i+1)); done; \
	if ! grep -q READY server.out 2>/dev/null; then \
		echo "FAIL: bash-pipes ($(HC)): server never became ready"; \
		cat server.out; exit 1; \
	fi
	@got=$$(printf "banana\napple\ncherry\n" | ./client | grep A | wc -l); \
	if [ "$$got" = "2" ]; then \
		echo "PASS: bash-pipes ($(HC))"; \
	else \
		echo "FAIL: bash-pipes ($(HC)): expected 2, got '$$got'"; \
		cat server.out; \
		exit 1; \
	fi

clean:
	rm -rf server client dist-ghc *.hi *.o server.out
