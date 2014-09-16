/*
 *  typespeed - measures your typing speed
 *  Copyright (C) 1999-2003   Jani Ollikainen  <bestis@iki.fi>
 *                          & Jaakko Manelius  <jman@iki.fi>
 *  Copyright (C) 2006-2008   Tobias Stoeckmann  <tobias@bugol.de>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * network.c - network for typespeed
 *
 * thanks goto Steinar H. Gunderson <sgunderson@bigfoot.com>
 * for the basic functions which we got from him
 */

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_SYS_IOCTL_H
	#include <sys/ioctl.h>
#endif /* HAVE_SYS_IOCTL_H */

#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#ifdef HAVE_SYS_SOCKET_H
	#include <sys/socket.h>
#endif /* HAVE_SYS_SOCKET_H */

#ifdef HAVE_NETINET_IN_H
	#include <netinet/in.h>
#endif /* HAVE_NETINET_IN_H */

#ifdef HAVE_ARPA_INET_H
	#include <arpa/inet.h>
#endif /* HAVE_ARPA_INET_H */

#include <ctype.h>
#include <curses.h>
#include <errno.h>

#ifdef HAVE_LIMITS_H
	#include <limits.h>
#endif /* HAVE_LIMITS_H */

#ifdef HAVE_NETDB_H
	#include <netdb.h>
#endif /* HAVE_NETDB_H */

#include <stdio.h>

#ifdef HAVE_STDARG_H
	#include <stdarg.h>
#endif /* HAVE_STDARG_H */

#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
	#include <string.h>
#endif /* HAVE_STRING_H */

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef WIN32
	#include <winsock2.h>

	#define EWOULDBLOCK	WSAEWOULDBLOCK
#endif /* WIN32 */

#include "gettext.h"
#include "typespeed.h"

#define _(string)	gettext(string)

#define IN_PREP	0
#define IN_GAME	1

extern int	addnewword(char *);
extern void	pressanykey(int, int);
extern int	r(int);
extern void	xerr(int, const char *, ...);
extern void	xerrx(int, const char *, ...);
extern int	xsnprintf(char *, size_t, const char *, ...);
extern void	xstrncpy(char *, char *, size_t);

void		closenetwork(void);
static void	gprint(int, int, int, const char *, ...);
static int	handle_command(char *);
int		initnetwork(char *, int);
static int	netident(int);
int		netrecv(int, int, int, int, char *, size_t);
int		netsend(char *);
int		netsendscore(struct stats *, char *);
int		netswapscore(struct stats *, struct stats *);
static uint32_t	readnum(char **);
void		setnoblock(void);
void		writenum(char *, uint32_t);

extern FILE	*netlogfile;

static int	 ds = -1;
static int	 ss = -1;
static int	 stat = IN_PREP;

#ifdef WIN32
static WSADATA wsaData;

int
inet_aton(const char *str, struct in_addr *add)
{
	add->s_addr = inet_addr(str);
	if (add->s_addr == INADDR_NONE)
		return 0;
	return 1;
}
#endif /* WIN32 */

/*
 * Closes network sockets and sets global ss = 0 if ss
 * has been initialized.
 */
void
closenetwork()
{
	if (ss != -1) {
#ifdef WIN32
		closesocket(ss);
#else
		close(ss);
#endif /* WIN32 */
		ss = -1;
	}

	if (ds != -1) {
#ifdef WIN32
		closesocket(ds);
#else
		close(ds);
#endif /* WIN32 */
		ds = -1;
	}

#ifdef WIN32
	WSACleanup();
#endif /* WIN32 */

	if (netlogfile != NULL)
		fputs("--- Network closed ---\n", netlogfile);

	opt.net = 0;
}

/*
 * Wrapper function that prints line at location x y if curses has
 * been initialized. If it is not, output is print on stdout.
 */
static void
gprint(int graph, int y, int x, const char *fmt, ...)
{
	int n;
	va_list ap;
	char *p;
	char output[80];

        va_start(ap, fmt);
	if (graph) {
		n = vsnprintf(output, sizeof(output), fmt, ap);
		if (n < 1 || (size_t)n > sizeof(output) - 1)
			xerrx(1, "malformed line to be written: %s", output);
		if ((p = strchr(output, '\n')) != NULL)
			*p = '\0';
		mvaddstr(y, x, output);
		refresh();
	} else
		vfprintf(stdout, fmt, ap);
	va_end(ap);
}

static int
handle_command(char *cmd)
{
	switch(stat) {
	case IN_PREP:
		if (!strcmp(cmd, "GAME START")) {
			stat = IN_GAME;
			return 5;
		}
		if (!strncmp(cmd, "VERSION ", sizeof("VERSION ") - 1))
			return 3;
		if (!strncmp(cmd, "RULESET ", sizeof("RULESET ") - 1))
			return 6;
		if (!strncmp(cmd, "RULESETS ", sizeof("RULESETS ") - 1))
			return 7;
		if (!strncmp(cmd, "WORDLIST ", sizeof("WORDLIST ") - 1))
			return 8;
		if (!strncmp(cmd, "WORDLISTS ", sizeof("WORDLISTS ") - 1))
			return 9;
		if (!strncmp(cmd, "FILELIST ", sizeof("FILELIST ") - 1))
			return 10;
		if (!strncmp(cmd, "SCORES ", sizeof("SCORES ") - 1))
			return 11;
		if (!strncmp(cmd, "SCORE ", sizeof("SCORE ") - 1))
			return 4;
		break;
	case IN_GAME:
		if (!strcmp(cmd, "GAME END")) {
			stat = IN_PREP;
			return 2;
		}
		break;
	default:
		break;
	}
	return -1;
}

/*
 * In this function, client and server are set up. Output must be possible
 * on command line and curses mode, because command line options can be
 * passed to typespeed - this function would be called before cursesinit.
 * If opt.net = CLIENT|THINCL, client specific code will be executed. In
 * this case, serv will be contacted.
 * "graph" tells initnetwork if graphic mode is enabled. If this is the
 * case, initnetwork will also use non-blocking mode during connection
 * setup. This way, a user can abort it and reach menu again.
 *
 * Returns 0 on success and -1 on error.
 */
int
initnetwork(char *serv, int graph)
{
	int one;
	struct sockaddr_in addr;

	one = 1;

#ifdef WIN32
	if (WSAStartup(MAKEWORD(1, 1), &wsaData) != 0)
		xerrx(1, "initnetwork: WSAStartup failed");
#endif /* WIN32 */

	memset(&addr, 0, sizeof(addr));
	addr.sin_family = PF_INET;
	addr.sin_port = htons(opt.port);

	stat = IN_PREP;

	if (serv[0] != '\0') {
		/* (thin) client code */
		if ((ds = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1)
			xerr(1, "initnetwork: socket");

		if (!inet_aton(serv, &addr.sin_addr)) {
			struct hostent *he;

			gprint(graph, 10, 30, _("Looking up %s...\n"), serv);

			if ((he = gethostbyname(serv)) == NULL) {
				gprint(graph, 12, 30, _("Cannot lookup %s\n"),
				    serv);
				if (graph)
					pressanykey(14, 30);
				closenetwork();
				return -1;
			}
			memcpy(&addr.sin_addr, he->h_addr, he->h_length);
		}

		gprint(graph, 10, 30, _("Connecting to typespeed server at "));
		gprint(graph, 11, 30, _("%s (port %d)...\n"),
		    inet_ntoa(addr.sin_addr), opt.port);

		if (connect(ds, (struct sockaddr *)&addr, sizeof(addr))) {
			closenetwork();
			gprint(graph, 13, 30, _("Cannot connect"));
			if (graph) {
				nocbreak();
				cbreak();
				pressanykey(15, 30);
			}
			return -1;
		}

		if (graph) {
			setnoblock();
			halfdelay(1);
		}
	} else {
		/* server code */
		opt.net = H2H;

		addr.sin_addr.s_addr = INADDR_ANY;

		if ((ss = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1)
			xerr(1, "initnetwork: socket");

		setsockopt(ss, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
		if (bind(ss, (struct sockaddr *)&addr,
		    sizeof(struct sockaddr)) == -1)
			xerr(1, "initnetwork: bind");

		if (listen(ss, 1))
			xerr(1, "initnetwork: listen");

		if (graph &&
#ifdef WIN32
		    ioctlsocket(ss, FIONBIO, &one)
#else
		    ioctl(ss, FIONBIO, &one)
#endif /* WIN32 */
		    )
			xerr(1, "initnetwork: ioctl");

		gprint(graph, 10, 30, _("Waiting for connections...\n"));

		if (graph)
			halfdelay(1);

		while ((ds = accept(ss, INADDR_ANY, NULL)) == -1)
			if (graph &&
#ifdef WIN32
			    GetLastError()
#else
			    errno
#endif /* WIN32 */
			    == EWOULDBLOCK) {
				if (getch() != ERR) {
					closenetwork();
					nocbreak();
					cbreak();
					return -1;
				}
			} else
				xerr(1, "initnetwork: accept");

		setnoblock();
	}

	if (graph) {
		nocbreak();
		cbreak();
	}

	if (netident(graph)) {
		closenetwork();
		return -1;
	}

	if (netlogfile != NULL)
		fputs("--- Network initialized ---\n", netlogfile);

	return 0;
}

/*
 * Sends and receives typespeed identification string.
 *
 * Returns 0 on success (remote uses typespeed and connection successful)
 * or -1 on error.
 */
static int
netident(int graph)
{
	char *p;
	char buf[1024];

	if (xsnprintf(buf, sizeof(buf), " VERSION H2H %s", TVERSION))
		xerrx(1, "netident: malformed version");

	if (netsend(buf))
		return -1;

	gprint(graph, 10, 30, _("Waiting for other party to identify...\n"));

	switch (netrecv(1, 1, graph, 1, buf, sizeof(buf))) {
	case 3:
		p = buf + sizeof(" VERSION ") - 1;
		if (!strncmp(p, "H2H ", sizeof("H2H ") - 1)) {
			opt.net = H2H;
			p += sizeof("H2H ") - 1;
		} else if (!strncmp(p, "NET ", sizeof("NET ") - 1)) {
			opt.net = NET;
			p += sizeof("NET ") - 1;
		} else
			break;
		gprint(graph, 13, 30, _("Remote is: %s\n"), p);
		if (strcmp(p, TVERSION)) {
			gprint(graph, 15, 30, _("Version missmatch\n"));
			closenetwork();
			if (graph)
				pressanykey(17, 30);
			return -1;
		}
		return 0;
		/* NOTREACHED */
		break;
	default:
		break;
	}

	gprint(graph, 13, 30, _("Remote is not typespeed\n"));
	if (graph)
		pressanykey(17, 30);
	return -1;
}

/*
 * Interprets incoming words.
 * If p is not NULL, '\0' will be written into p on error (if no valid
 * word has been parsed) or the received word, whereas not more than
 * n bytes will be used (including '\0').
 *
 * Returns:
 * -1 if string without \n has been found
 *  0 if no \n terminated string could be found
 *  1 if word has been found and added
 *  2 if network should be shut down
 *  3 if received word matches " VERSION "
 *  4 if received word matches " SCORE "
 *  5 if received word matches " STARTGAME"
 */
int
netrecv(int block, int delay, int graph, int handle, char *p, size_t n)
{
	int retval;
	ssize_t i;
	size_t len;
	char *q;
	char buf[1024];

	buf[0] = '\0';
	retval = 0;

	if (delay && graph)
		halfdelay(1);

	do {
		i = recv(ds, buf, sizeof(buf), MSG_PEEK);
		buf[sizeof(buf) - 1] = '\0';

		if (i == -1 &&
#ifdef WIN32
		    GetLastError()
#else
		    errno
#endif /* WIN32 */
		    != EWOULDBLOCK) {
			retval = -1;
			break;
		}
		if (i < 1)
			goto keypress;

		if ((q = strchr(buf, '\n')) == NULL) {
			/*
			 * A string without trailing newline and max line
			 * length is invalid: clear buffer and return -1.
			 */
			if (strlen(buf) == sizeof(buf) - 1) {
				recv(ds, buf, sizeof(buf), 0);
				retval = -1;
			}
			goto keypress;
		}

		*(q + 1) = '\0';
		len = strlen(buf);
		recv(ds, buf, len + 1, 0);
		*q = '\0';

		if (netlogfile != NULL)
			fprintf(netlogfile, "<< %s\n", buf);

		if (handle) {
			if (buf[0] == ' ')
				retval = handle_command(buf + 1);
			else if (stat == IN_GAME) {
				addnewword(buf);
				retval = 1;
			} else
				retval = -1;
		} else {
			retval = -2;
			continue;
		}

keypress:
		if (block && graph && getch() == 27) /* ESC */
			retval = -1;
	} while (block && !retval);

	if (retval == -1) {
		/* be nice and try to send ERROR */
		(void)netsend(" ERROR");
		closenetwork();
	}

	if (p != NULL && n)
		xstrncpy(p, buf, n - 1);

	if (delay && graph) {
		nocbreak();
		cbreak();
	}

	return retval;
}

/*
 * Sends word "wordstring" through socket "ds" to opponent.
 */
int
netsend(char *wordstring)
{
	char buf[1024];
	char *p;
	int i;
	size_t len;

	strncpy(buf, wordstring, sizeof(buf) - 2);
	strncat(buf, "\n", sizeof(buf) - 1 - strlen(buf));
	buf[sizeof(buf) - 1] = '\0';

	for (p = buf, len = strlen(buf) + 1; len;) {
		if ((i = send(ds, p, len, 0)) == -1) {
			if (
#ifdef WIN32
			    GetLastError()
#else
			    errno
#endif /* WIN32 */
			    == EAGAIN)
				continue;
			closenetwork();
			return -1;
		}
		len -= i;
		p += i;
	}

	if (netlogfile != NULL)
		fprintf(netlogfile, ">> %s\n", wordstring);

	return 0;
}

int
netsendscore(struct stats *stat, char *name)
{
	char *p;
	char buf[sizeof(" SCORE ") + 80];

	xstrncpy(buf, " SCORE ", sizeof(buf) - 1);
	p = buf + sizeof(" SCORE ") - 1;

	/* score */
	writenum(p, stat->score);
	p += 11;

	/* tcount */
	writenum(p, stat->tcount);
	p += 11;

	/* wordswritten */
	writenum(p, stat->wordswritten);
	p += 11;

	/* duration */
	writenum(p, stat->duration);
	p += 11;

	/* sinit */
	writenum(p, stat->sinit);

	/* name */
	strncat(buf, name, 20);
	buf[sizeof(buf) - 1] = '\0';

	if (netsend(buf))
		return -1;

	return 0;
}

/*
 * Converts current stat into network protocol text (a.k.a. simple string)
 * and sends to opponent. Next to this, opponent's score will be received.
 *
 * stat2 will be filled with opponent's score.
 */
int
netswapscore(struct stats *stat, struct stats *stat2)
{
	int i;
	size_t n;
	char *p;
	char buf[sizeof(" SCORE ") + 80];

	if (netsendscore(stat, opt.name))
		return -1;

	do {
		memset(buf, 0, sizeof(buf));
		i = netrecv(1, 1, 1, 1, buf, sizeof(buf));

		switch(i) {
		case 2:
		case 4:
			break;
		default:
			return -1;
			/* NOTREACHED */
			break;
		}
	} while (i != 4);

	p = buf + sizeof(" SCORE ") - 1;

	stat2->score = readnum(&p);
	if (p == NULL || *p == '\0')
		return -1;

	stat2->tcount = readnum(&p);
	if (p == NULL || *p == '\0')
		return -1;

	stat2->wordswritten = readnum(&p);
	if (p == NULL || *p == '\0')
		return -1;

	stat2->duration = readnum(&p);
	if (p == NULL || *p == '\0')
		return -1;

	stat2->sinit = readnum(&p);
	if (p == NULL || *p == '\0')
		return -1;

	for (n = 0; (n < sizeof(stat2->name) - 1) && *p != '\0'; n++, p++)
		if (isprint(*p))
			stat2->name[n] = *p;
		else
			return -1;
	if (*p != '\0')
		return -1;
	stat2->name[n] = '\0';

	if (stat2->duration)
		stat2->speed = (stat2->score + stat2->wordswritten) * 100.0 /
			stat2->duration;
	else
		stat2->speed = 0;

	stat2->wpm = stat2->speed * 12;

	if (stat2->tcount)
		stat2->ratio = (1 - stat2->score + stat2->wordswritten) /
			(stat2->tcount + stat2->wordswritten) * 100;
	else
		stat2->ratio = 0;

	return 0;
}

/*
 * Sets buf to NULL on error.
 */
static uint32_t
readnum(char **buf)
{
	char *number, *p;

	p = *buf;

	while (*p == ' ')
		p++;
	number = p;
	if (*p < '0' || *p > '9') {
		*buf = NULL;
		return 0;
	}
	while (*p >= '0' && *p <= '9')
		p++;
	if (*p != '\0')
		*p++ = '\0';

	*buf = p;
	return strtol(number, NULL, 10);
}

void
setnoblock()
{
	int one = 1;

	if (
#ifdef WIN32
	ioctlsocket(ds, FIONBIO, &one)
#else
	ioctl(ds, FIONBIO, &one)
#endif /* WIN32 */
	)
		xerr(1, "setnoblock: ioctl");
}

void
writenum(char *p, uint32_t number)
{
	int num, blank;
	uint32_t i;

	blank = 1;
	for (i = 1000000000; i; i /= 10, p++) {
		if ((num = number / i))
			blank = 0;
		if (!blank)
			*p = '0' + num;
		else
			*p = ' ';
		number -= num * i;
	}

	/*
	 * XXX blank == 1?
	 * If it's 0, we have to adjust it here.
	 */
	if (*(p - 1) == ' ')
		*(p - 1) = '0';
	*p++ = ' ';

	*p = '\0';
}

