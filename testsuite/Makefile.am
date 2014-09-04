CC = gcc
AM_CFLAGS = -I../src -Wall -W -pedantic -std=c99

localedir = $(datadir)/locale
AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\" -DTVERSION=\"v$(VERSION)\" -DTEST
LDADD = @LIBINTL@

EXTRA_DIST = t_unescstr.sh t_loadwords_data/3_empty

noinst_PROGRAMS = t_level t_loadwords t_typorankkaus t_unescstr

t_level_SOURCES= misc-ctest.c t_level.c
nodist_t_level_SOURCES = file.c menu.c network.c typespeed.c

t_loadwords_SOURCES = t_loadwords.c
nodist_t_loadwords_SOURCES =  file.c menu.c misc.c network.c typespeed.c

t_typorankkaus_SOURCES = misc-ctest.c t_typorankkaus.c
nodist_t_typorankkaus_SOURCES = file.c menu.c network.c typespeed.c

t_unescstr_SOURCES = t_unescstr.c
nodist_t_unescstr_SOURCES = file.c menu.c misc.c network.c typespeed.c

file.o: ../src/file.c ../src/pathnames.h ../src/typespeed.h
	$(COMPILE) -c ../src/file.c

menu.o: ../src/menu.c ../src/typespeed.h
	$(COMPILE) -c ../src/menu.c

misc.o: ../src/misc.c ../src/typespeed.h
	$(COMPILE) -c ../src/misc.c

misc-ctest.o: ../src/misc.c ../src/typespeed.h
	$(COMPILE) -DCOLORTEST -c -o misc-ctest.o ../src/misc.c

network.o: ../src/network.c ../src/typespeed.h
	$(COMPILE) -c ../src/network.c

typespeed.o: ../src/typespeed.c
	$(COMPILE) -DTEST -c ../src/typespeed.c

