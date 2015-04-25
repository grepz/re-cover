CC = ./rebar

all: deps re-cover

deps:
	$(CC) get-deps

re-cover:
	$(CC) compile

clean:
	$(CC) clean
