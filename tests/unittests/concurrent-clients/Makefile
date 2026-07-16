# Starts one server and N clients launched at (roughly) the same time,
# and checks every client got the reply meant for it. Concurrency here
# is at the OS-process level (N separate client binaries backgrounded
# from the shell) rather than forkIO inside one process -- see
# so-reuseaddr-churn/Makefile for why that matters under GHC.
#
# Usage:
#   make test              # build and run with mhs (default)
#   make test HC=ghc       # build and run with GHC, against the library source
#   make clean

.PHONY: all test clean

HC       ?= mhs
TOPSRC   := ../../../src
NCLIENTS := 8

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
	@rm -f server.out client-*.out
	@( timeout 20 ./server $(NCLIENTS) > server.out 2>&1 & )
	@i=0; \
	while [ $$i -lt 50 ] && ! grep -q READY server.out 2>/dev/null; do sleep 0.1; i=$$((i+1)); done; \
	if ! grep -q READY server.out 2>/dev/null; then \
		echo "FAIL: concurrent-clients ($(HC)): server never became ready"; \
		cat server.out; exit 1; \
	fi
	@for i in $$(seq 1 $(NCLIENTS)); do \
		./client $$i > client-$$i.out 2>&1 & \
	done; \
	wait
	@sleep 0.3
	@ok=1; \
	for i in $$(seq 1 $(NCLIENTS)); do \
		grep -q "SERVER_GOT:client-$$i$$" server.out || { echo "FAIL: server never saw client-$$i"; ok=0; }; \
		grep -q "CLIENT_$${i}_GOT:ack:client-$$i$$" client-$$i.out || { echo "FAIL: client $$i got the wrong (or no) reply"; ok=0; }; \
	done; \
	grep -q SERVER_DONE server.out || { echo "FAIL: server did not finish"; ok=0; }; \
	if [ $$ok -eq 1 ]; then \
		echo "PASS: concurrent-clients ($(HC))"; \
	else \
		echo "--- server.out ---"; cat server.out; \
		for i in $$(seq 1 $(NCLIENTS)); do echo "--- client-$$i.out ---"; cat client-$$i.out; done; \
		exit 1; \
	fi

clean:
	rm -rf server client dist-ghc *.hi *.o server.out client-*.out
