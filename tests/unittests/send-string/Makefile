# Integration test: a server accepts one connection, echoes back an
# acknowledgement of whatever string the client sent, and the harness
# checks that both sides saw the expected bytes.
#
# Usage:
#   make test              # build and run with mhs (default)
#   make test HC=ghc       # build and run with GHC, against the library source
#   make clean

.PHONY: all test clean

HC     ?= mhs
TOPSRC := ../../../src

EXPECT_SERVER := SERVER_GOT:hello-network-light
EXPECT_CLIENT := CLIENT_GOT:ack:hello-network-light

all: test

ifeq ($(HC),mhs)
# -i points straight at the in-tree library source, so the test always
# exercises the current checkout instead of whatever network-light build
# last happened to be installed into the shared mcabal package cache.
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
	@rm -f server.out client.out
	@( timeout 5 ./server > server.out 2>&1 & )
	@i=0; \
	while [ $$i -lt 50 ] && ! grep -q READY server.out 2>/dev/null; do \
		sleep 0.1; i=$$((i+1)); \
	done; \
	if ! grep -q READY server.out 2>/dev/null; then \
		echo "FAIL: send-string ($(HC)): server never became ready"; \
		cat server.out; \
		exit 1; \
	fi
	@./client > client.out 2>&1
	@sleep 0.2
	@if grep -q "$(EXPECT_SERVER)" server.out && grep -q "$(EXPECT_CLIENT)" client.out; then \
		echo "PASS: send-string ($(HC))"; \
	else \
		echo "FAIL: send-string ($(HC))"; \
		echo "--- server.out ---"; cat server.out; \
		echo "--- client.out ---"; cat client.out; \
		exit 1; \
	fi

clean:
	rm -rf server client dist-ghc *.hi *.o server.out client.out
