#makefile for both game and scorescreen

all:
	cd game && ./configure && make all install && cd ../scorescreen $$ ghc main.hs
