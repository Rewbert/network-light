.PHONY: all clean

all:	server client serverap clientap

server: Server.hs
	mhs Server -o server

client: Client.hs
	mhs Client -o client

serverap: ServerApply.hs
	mhs ServerApply -o serverap

clientap: ClientApply.hs
	mhs ClientApply -o clientap

clean:
	rm -rf server client serverap clientap *~
