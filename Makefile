#makefile for both game and scorescreen

all:
	cd game && ./configure && make all install && cd ../scorescreen && ghc -o /usr/local/bin/Scorescreen main.hs
