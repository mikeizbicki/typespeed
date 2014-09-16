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
 * misc.c - misc functions
 */

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_SYS_TIME_H
	#include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */

#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#include <curses.h>
#include <ctype.h>
#include <errno.h>

#ifdef HAVE_STDARG_H
	#include <stdarg.h>
#endif /* HAVE_STDARG_H */

#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
	#include <string.h>
#endif /* HAVE_STRING_H */

#include <time.h>

#include "gettext.h"
#include "typespeed.h"

#define _(string)	gettext(string)

unsigned long	 cstrl(char *);
void		 endcursestuff(void);
void		 initcursestuff(void);
unsigned short	 level(int);
void		 liima_mvgetnstr(int, int, char *, int);
int		 r(int);
clock_t		 timenow(void);
int		 typorankkaus(float);
void		 xcolor_set(short);
void		 xerr(int, const char *, ...);
void		 xerrx(int, const char *, ...);
void		*xmalloc(size_t);
int		 xsnprintf(char *, size_t, const char *, ...);
char		*xstrdup(char *);
void		 xstrncpy(char *, char *, size_t);

extern char	*progname;

static int graph = 0;

/*
 * Converts a string to an unsigned long integer.
 * The error variable errno will be overwritten!
 * Returns 0 on failure with errno != 0.
 */
unsigned long
cstrl(char *str)
{
	char *p;
	long val;

	errno = 0;
	val = strtol(str, &p, 10);
	if (errno)
		return 0;
	if (val < 0 || *p != '\0') {
		errno = EINVAL;
		return 0;
	}
	return (unsigned long)val;
}


/* Closes all relevant curses objects. */
void
endcursestuff(void)
{
	if (!graph)
		return;

	clear();
	refresh();
	endwin();

	graph = 0;
}

/* Initialises curses */
void
initcursestuff(void)
{
	int height, width;

	if (graph)
		return;

	height = width = 0;

	initscr();
	graph = 1;

	getmaxyx(stdscr, height, width);
	if (height < 24 || width < 80)
		xerrx(1, _("You need at least 80 x 24 terminal!"));

	keypad(stdscr, TRUE);
	noecho();
	cbreak();
	start_color();
	nodelay(stdscr, FALSE);
	flushinp();

	init_pair(1, COLOR_GREEN, COLOR_BLACK);
	init_pair(2, COLOR_WHITE, COLOR_BLACK);
	init_pair(3, COLOR_RED, COLOR_BLACK);
	init_pair(4, COLOR_MAGENTA, COLOR_BLACK);
	init_pair(5, COLOR_CYAN, COLOR_BLACK);
	init_pair(6, COLOR_BLUE, COLOR_BLACK);
	init_pair(7, COLOR_YELLOW, COLOR_BLACK);
}

/* Returns (0 - 10) and sets color according to "pointsit". */
unsigned short
level(int pointsit)
{
	int leveli;

	if (pointsit < 1)
		leveli = 0;
	else if ((leveli = pointsit / 100 + 1) > 10)
		leveli = 10;

	if (pointsit < 400)
		xcolor_set(6);
	else if (pointsit < 600)
		xcolor_set(1);
	else if (pointsit < 700)
		xcolor_set(7);
	else
		xcolor_set(3);

	return leveli;
}

/*
 * yet another purkka by jaakko ..
 * (no mvgetnstr support in jaakko's ncurses)
 */
void
liima_mvgetnstr(int y, int x, char *buf, int maxlen)
{
	int ch, curlen;

	curlen = 0;

	memset(buf, 0, maxlen + 1);
	for (ch = 0; ch != 10;) {
		mvaddstr(y, x + curlen, "      ");
		move(y, x);
		if (curlen)
			addstr(buf);
		refresh();

		switch (ch = getch()) {
		case KEY_BACKSPACE:
		case 4:		/* EOT */
		case 8:		/* BS */
		case 127:	/* DEL */
			if (curlen) {
				echochar(ch);
				curlen--;
				buf[curlen] = '\0';
			}
			break;
		case 10:
			break;
		case 27:	/* ESC */
			flushinp();
			break;
		default:
			if (curlen != maxlen && !iscntrl(ch))
				buf[curlen++] = ch;
			break;
		}
	}
}

/* Returns a random number between 0 and "range". */
int
r(int range)
{
	if (range < 1)
		xerrx(1, "range of 0 detected!");
#ifdef WIN32
	return (int)(rand() % range);
#else
	return (int)(random() % range);
#endif /* WIN32 */
}

/* Returns the current time in hundreds of seconds. */
clock_t
timenow(void)
{
	struct timeval tval;
	gettimeofday(&tval, NULL);
	return ((clock_t)((tval.tv_sec * 100) + (tval.tv_usec / 10000)));
}

/* Returns (0 - 11) and sets color appropiate to "typorate". */
int
typorankkaus(float typorate)
{
	int typorankki;
	/*
	 * if you get a negative typorate here, you must
	 * be as good as my friend xmunkki
	 *
	 * and btw. thanks to xmunkki for his
	 * "OverHumanly debugging without compiler"
	 */
	if (typorate < 0)
		typorankki = 0;
	else if (typorate == 0)
		typorankki = 1;
	else if (typorate < 2)
		typorankki = 2;
	else if (typorate < 4)
		typorankki = 3;
	else if (typorate < 6)
		typorankki = 4;
	else if (typorate < 8)
		typorankki = 5;
	else if (typorate < 11)
		typorankki = 6;
	else if (typorate < 15)
		typorankki = 7;
	else if (typorate < 20)
		typorankki = 8;
	else if (typorate < 30)
		typorankki = 9;
	else if (typorate < 50)
		typorankki = 10;
	else
		typorankki = 11;

	if (typorate < 6)
		xcolor_set(6);
	else if (typorate < 11)
		xcolor_set(1);
	else if (typorate < 20)
		xcolor_set(7);
	else
		xcolor_set(3);

	return typorankki;
}

#ifndef COLORTEST
/* Calls color_set if opt.usecolors is set. */
void
xcolor_set(short color_pair_number)
{
#ifdef color_set
	if (opt.usecolors == 1)
		color_set(color_pair_number, NULL);
#endif /* color_set */
}
#endif /* COLORTEST */

/*
 * Rework of err() so systems without err can display these
 * messages. Also, curses will be shut down correctly.
 */
void
xerr(int ret, const char *fmt, ...)
{
	int backup;
	va_list ap;

	backup = errno;
	endcursestuff();

	fprintf(stderr, "%s: ", progname);
	va_start(ap, fmt);
	if (fmt != NULL) {
		vfprintf(stderr, fmt, ap);
		fprintf(stderr, ": ");
	}
	va_end(ap);
	fprintf(stderr, "%s\n", strerror(backup));

	exit(ret);
}

/*
 * Rework of errx() so systems without errx can display these
 * messages. Also, curses will be shut down correctly.
 */
void
xerrx(int ret, const char *fmt, ...)
{
	va_list ap;

	endcursestuff();

	va_start(ap, fmt);
	if (fmt != NULL) {
		fprintf(stderr, "%s: ", progname);
		vfprintf(stderr, fmt, ap);
	}
	va_end(ap);
	fputc('\n', stderr);

	exit(ret);
}

/* Wrapper for malloc */
void *
xmalloc(size_t size)
{
	void *pointer;
	if ((pointer = malloc(size)) == NULL)
		xerr(1, "malloc");
	return pointer;
}

/*
 * Wrapper for snprintf:
 * Returns 0 on success or 1 on failure.
 */
int
xsnprintf(char *s, size_t n, const char *fmt, ...)
{
	va_list ap;
	int i;

	va_start(ap, fmt);
	i = vsnprintf(s, n, fmt, ap);
	va_end(ap);

	if (i < 0 || (size_t)i >= n)
		return 1;
	return 0;
}

/* Wrapper for strdup */
char *
xstrdup(char *string)
{
	char *pointer;
	if ((pointer = strdup(string)) == NULL)
		xerr(1, "strdup");
	return pointer;
}

/* Wrapper for strncpy */
void
xstrncpy(char *dst, char *src, size_t n)
{
	(void)strncpy(dst, src, n);
	dst[n] = '\0';
}

