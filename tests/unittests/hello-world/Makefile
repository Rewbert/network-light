# Minimal integration test: a server accepts one connection and prints
# whatever the client sent. No handshake protocol between the two --
# just a short sleep to give the server time to start listening before
# the client connects.
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
	@( timeout 5 ./server > server.out 2>&1 & )
	@sleep 0.2
	@./client
	@sleep 0.2
	@if grep -q "hello world" server.out; then \
		echo "PASS: hello-world ($(HC))"; \
	else \
		echo "FAIL: hello-world ($(HC))"; \
		cat server.out; \
		exit 1; \
	fi

clean:
	rm -rf server client dist-ghc *.hi *.o server.out
