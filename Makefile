#makefile for both game and scorescreen

all:
	./copyFiles && cd game && ./configure && make all install && cd ../scorescreen && ghc -o /usr/local/bin/Scorescreen main.hs
