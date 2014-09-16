CC = gcc
AM_CFLAGS = -Wall -W -pedantic -std=c99

HIGHFILE = typespeed.score
WORDDIR = $(datadir)/@PACKAGE@/words
RULEDIR = $(datadir)/@PACKAGE@/rules

bin_PROGRAMS = typespeed
noinst_PROGRAMS = convert

highscore_DATA = typespeed.score

convert_SOURCES = convert.c

typespeed_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\" -DTVERSION=\"v$(VERSION)\" \
	-DCONFIGFILE=\"$(sysconfdir)/typespeedrc\" \
        -DHIGHFILE=\"$(highscoredir)/$(HIGHFILE)\" \
	-DWORDDIR=\"$(WORDDIR)\" -DRULEDIR=\"$(RULEDIR)\"
typespeed_LDADD = @LIBINTL@
typespeed_SOURCES = file.c gettext.h menu.c misc.c network.c pathnames.h \
	    typespeed.c typespeed.h

typespeed.score:
	@touch typespeed.score

install-data-hook:
	-chgrp games $(DESTDIR)$(highscoredir)/$(HIGHFILE) && \
	chmod g+w $(DESTDIR)$(highscoredir)/$(HIGHFILE) && \
	chmod o-rwx $(DESTDIR)$(highscoredir)/$(HIGHFILE)

install-exec-hook:
	-chgrp games $(DESTDIR)$(bindir)/typespeed && \
	chmod g+s $(DESTDIR)$(bindir)/typespeed
