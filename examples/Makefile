.PHONY: all clean

all:	server client

server: Server.hs
	mhs Server -o server

client: Client.hs
	mhs Client -o client

clean:
	rm -rf server client *~
