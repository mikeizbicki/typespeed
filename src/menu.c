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
 * menu.c - menu & misc drawing functions
 */

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_SYS_PARAM_H
	#include <sys/param.h>
#endif /* HAVE_SYS_PARAM_H */

#include <curses.h>
#include <ctype.h>
#include <errno.h>

#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
	#include <string.h>
#endif /* HAVE_STRING_H */

#include "gettext.h"
#include "typespeed.h"

#define _(string)	gettext(string)

extern int		chooseruleset(void);
extern int		choosewordfile(int);
extern void		closenetwork(void);
extern unsigned long	cstrl(char *);
extern void		xerrx(int, const char *, ...);
extern int		initnetwork(char *, int);
extern unsigned short	level(int);
extern void		liima_mvgetnstr(int, int, char *, int);
extern void		multipmenu(void);
extern void		xcolor_set(short);

static int	 dochange(int);
void		 drawmenu(void);
void		 drawscreen(void);
void		 drawstatus(unsigned);
int		 fileselmenu(int, struct finfo *, const char *);
static int	 getnum(const char *, unsigned long, unsigned long);
void		 initstatus(char *);
void		 multipmenu(void);
void		 nowordlist(void);
void		 optionmenu(void);
void		 pressanykey(int, int);
void		 rulesmenu(void);
void		 showhighscores(void);
void		 tellstory(void);

extern int	 misses;
extern char	*rankki[11];

struct positions {
	int rank;
	int score;
	int wpm;
	int speed;
	int misses;
} pos;

/* Switches "number" from 1 to 0 or any other value to 1. */
static int
dochange(int number)
{
	return (number == 1) ? 0 : 1;
}

/* Draws main menu onto screen. */
void
drawmenu(void)
{
	drawscreen();

	mvaddstr( 3, 30, "Typespeed ");
	addstr(TVERSION);

	mvaddstr( 5, 30, _("1. Test Your Speed"));
	if (opt.net == NET)
		mvaddstr( 6, 30, _("2. Close Connection"));
	else
		mvaddstr( 6, 30, _("2. Network Head2Head"));
	mvaddstr( 7, 30, _("3. Story/Credits/RTFM!"));
	mvaddstr( 8, 30, _("4. Show Highscores"));
	mvaddstr( 9, 30, _("5. Options"));
	mvaddstr(10, 30, _("6. Game Rules"));
	mvaddstr(11, 30, _("7. Quit"));

	mvaddstr(13, 30, _("Choose: "));
}

/* Clears screen and draws status bar. */
void
drawscreen(void)
{
	clear();

	xcolor_set(4);
	move(22, 0);
	hline(ACS_HLINE, 80);
	mvaddch(23, 1, '>');
	mvaddch(23, 21, '<');

	xcolor_set(5);
	mvaddstr(23, 23,
	    _("Rank: %------- Score: %--- WPM: %-- CPS: %---- Misses: %-"));

	/* sets color to 2: NET, CHE need this */
	drawstatus(0);

	switch (opt.net) {
	case H2H:
		mvaddstr(22, 70, "H2H");
		break;
	case NET:
		mvaddstr(22, 70, "NET");
		break;
	default:
		break;
	}

	if (opt.cheat)
		mvaddstr(22, 73, "CHE");

	xcolor_set(5);
}

/*
 * Prints status values - no descriptions though!
 * Cursor will be put into "inputpos" (y = 21, x = 2 + inputpos).
 */
void
drawstatus(unsigned inputpos)
{
	mvprintw(23, pos.rank, "%-9s", rankki[level(now.score)]);

	xcolor_set(3);
	mvprintw(23, pos.score, "%4lu", now.score);
	mvprintw(23, pos.wpm, "%3lu", now.wpm);
	mvprintw(23, pos.speed, now.speed < 10.0f ? " %2.2f" : "%2.2f",
	    now.speed);
	mvprintw(23, pos.misses, "%2lu", misses);

	xcolor_set(2);
	move(23, 2 + inputpos);
}

/* Prints file selection menu. */
int
fileselmenu(int tot, struct finfo *thingie, const char *title)
{
	int a, cpos, k;

	drawscreen();
	xcolor_set(4);
    /*mvprintw(2, 5, _("Choose a %s (UP/DOWN/ENTER):"), title);*/
    mvprintw(2, 5, _(""));
	if (tot > 17)
		for (k = 0; k < 17; k++)
			mvaddstr(k + 4, 5, thingie[k].descr);
	else
		for (k = 0; k < tot; k++)
			mvaddstr(k + 4, 5, thingie[k].descr);

	cpos = k = 0;
	mvaddstr(4, 1, "->");

	for (a = 1; a;) {
		/*
		 * And thanks for xmunkki, he made my day and
		 * "solved" this, what i didn't get right (or did
		 * but i used 2 extra variables. :)
		 */
		switch (getch()) {
		case 27:
			return -1;
			break;
		case KEY_UP:
		case 'k':
			if (k <= 0)
				break;
			k--;
			if (cpos > 0) {
				cpos--;
				break;
			}
			move(20, 5);
			deleteln();
			move(4, 5);
			insertln();
			mvaddstr(4, 5, thingie[k].descr);
			break;
		case KEY_DOWN:
		case 'j':
			if (k > tot - 2)
				break;
			k++;
			if (cpos < 16 && cpos < tot - 1) {
				cpos++;
				break;
			}
			move(4, 5);
			deleteln();
			move(20, 5);
			insertln();
			mvaddstr(20, 5, thingie[k].descr);
			break;
		case ' ':
		case '\n':
			a = 0;
			break;
		default:
			break;
		}
		mvaddstr(cpos + 3, 1, "  ");
		mvaddstr(cpos + 5, 1, "  ");
		mvaddstr(cpos + 4, 1, "->");
	}

	return k;
}

/*
 * Asks player about a specific value named by "string". If the entered
 * value is smaller or equal min, or larger or equal max, 0 is returned.
 * If a string (and not a number) has been entered, 0 is returned, too.
 *
 * Sets rules.hightype to 0, so a customized rule set cannot reach top10.
 */
static int
getnum(const char *string, unsigned long min, unsigned long max)
{
	unsigned long res;
	char entr[11];
	char clr[80];

	memset(clr, ' ', sizeof(clr));
	clr[sizeof(clr) - 1] = '\0';

	mvaddstr(17, 0, clr);
	mvaddstr(17, 30, string);
	liima_mvgetnstr(17, 31 + strlen(string), entr, sizeof(entr) - 1);
	entr[sizeof(entr) - 1] = '\0';

	if (!(res = cstrl(entr)) && errno)
		return 0;
	if (res > min && res < max) {
		rules.hightype = 0;
		return res;
	}
	return 0;
}

/*
 * Note places where rank, score, wpm, cps, and misses have to
 * be printed, so translators have a much easier job.
 *
 * "Rank: %------- Score: %--- WPM: %-- CPS: %----  Misses: %-"
 */
void
initstatus(char *str)
{
	int field;
	char *p;

	for (field = 0, p = str; *p != '\0'; p++) {
		if (*p != '%')
			continue;

		switch (field) {
		case 0:
			pos.rank = p - str + 23;
			break;
		case 1:
			pos.score = p - str + 23;
			break;
		case 2:
			pos.wpm = p - str + 23;
			break;
		case 3:
			pos.speed = p - str + 23;
			break;
		case 4:
			pos.misses = p - str + 23;
			break;
		default:
			xerrx(1, "1wrong status format");
			/* NOTREACHED */
		}
		field++;
	}

	if (field != 5)
		xerrx(1, "2wrong status format");
}

/* Draws multiplayer menu. */
void
multipmenu(void)
{
	int exitnow;
	char port[6], serv[MAXHOSTNAMELEN];

	exitnow = 0;
	serv[0] = '\0';

	do {
		drawscreen();
		mvaddstr(3, 30, "Typespeed ");
		addstr(TVERSION);

		mvaddstr( 5, 30, _("1. Server"));
		mvaddstr( 6, 30, _("2. Client"));
		mvaddstr( 7, 30, _("3. Port"));
		mvaddstr( 8, 30, _("4. Player Name"));
		mvaddstr( 9, 30, _("5. Return"));

		xcolor_set(5);
		mvprintw( 7, 50, "%d", opt.port);
		mvprintw( 8, 50, "%s", opt.name);
		mvaddstr(11, 30, _("Choose: "));

		switch (getch()) {
		case '1':
			serv[0] = '\0';
			exitnow = 1;
			break;
		case '2':
			mvaddstr(11, 30, _("Enter Host:"));
			liima_mvgetnstr(11, 31 +
			    strlen(_("Enter Host:")), serv,
			    sizeof(serv) - 1);
			serv[sizeof(serv) - 1] = '\0';
			exitnow = 1;
			break;
		case '3':
			mvaddstr(11, 30, _("Enter Port:"));
			liima_mvgetnstr(11, 31 + strlen(_("Enter Port:")), port,
			    sizeof(port) - 1);
			port[sizeof(port) - 1] = '\0';
			opt.port = strtol(port, NULL, 10);
			if (opt.port <= 1024)
				opt.port = 6025;
			exitnow = 0;
			break;
		case '4':
			mvaddstr(11, 30, _("Enter Name:"));
			liima_mvgetnstr(11, 31 +
			    strlen(_("Enter Name:")), opt.name,
			    sizeof(opt.name) - 1);
			opt.name[sizeof(opt.name) - 1] = '\0';
			exitnow = 0;
			break;
		case '5':
			return;
			/* NOTREACHED */
			break;
		default:
			break;
		}
	} while (!exitnow);

	drawscreen();
	mvaddstr(3, 30, "Typespeed ");
	addstr(TVERSION);

	mvaddstr(5, 30, _("When you get connect, choose a word list and"));
	mvaddstr(6, 30, _("then wait for the game start. The game starts as"));
	mvaddstr(7, 30, _("soon as the other player has chosen a word list."));
	refresh();

	initnetwork(serv, 1);
	flushinp();
}

/*
 * Prints "No word lists found...".
 * This function just exists to split all output out of file.c.
 */
void
nowordlist(void)
{
	drawscreen();
	mvaddstr(1, 5, _("No word lists found..."));
	pressanykey(3, 5);
}

/* Draws option menu. */
void
optionmenu(void)
{
	int wheretogo;

	do {
		drawscreen();

		mvaddstr(3, 30, "Typespeed ");
		addstr(TVERSION);

		mvaddstr(5, 30, _("1. Colors"));
		mvaddstr(6, 30, _("2. Cheat"));
		mvaddstr(7, 30, _("3. Return"));

		xcolor_set(3);
		if (opt.usecolors)
			mvaddstr(5, 44, _("ON"));
		if (opt.cheat)
			mvaddstr(6, 44, _("ON"));

		xcolor_set(6);
		if (!opt.usecolors)
			mvaddstr(5, 44, _("OFF"));
		if (!opt.cheat)
			mvaddstr(6, 44, _("OFF"));

		xcolor_set(5);
		mvaddstr(12, 30, _("Choose: "));

		switch (wheretogo = getch()) {
		case '2':
			opt.cheat = dochange(opt.cheat);
			break;
		case '1':
			if (opt.usecolors)
				xcolor_set(2);
			opt.usecolors = dochange(opt.usecolors);
			break;
		default:
			break;
		}
	} while (wheretogo != '3');
}

/* This function waits until a key has been pressed */
void
pressanykey(int y, int x)
{
	flushinp();
	xcolor_set(4);
	mvaddstr(y, x, _("Press any key to continue..."));
	getch();
}

/* Draws rules menu. */
void
rulesmenu(void)
{
	int wheretogo;
	long tmp;

	do {
		drawscreen();

		mvaddstr( 3, 30, "Typespeed ");
		addstr(TVERSION);

		mvaddstr( 5, 30, _("Current Rule Set:"));

		mvaddstr( 7, 30, _("1. Load Another Rule Set"));
		mvaddstr( 8, 30, _("2. Misses Limit:"));
		mvaddstr( 9, 30, _("3. Word Length:"));
		addstr("    -");
		mvaddstr(10, 30, _("4. Words On Screen:"));
		addstr("    -");
		mvaddstr(11, 30, _("   High Score Enabled:"));
		mvaddstr(12, 30, _("6. Speed Step:"));
		mvaddstr(13, 30, _("7. Speed Range:"));
		addstr("    -");
		mvaddstr(14, 30, _("8. Smoothness:"));
		mvaddstr(15, 30, _("9. Return"));

		xcolor_set(3);
		mvprintw( 5, 31 + strlen(_("Current Rule Set:")), "%s",
		    rules.name);
		mvprintw( 8, 31 + strlen(_("2. Misses Limit:")), "%d",
		    rules.misses);
		mvprintw( 9, 31 + strlen(_("3. Word Length:")), "%2d",
		    rules.minlen);
		mvprintw( 9, 36 + strlen(_("3. Word Length:")), "%2d",
		    rules.maxlen);
		mvprintw(10, 31 + strlen(_("4. Words On Screen:")), "%2d",
		    rules.minwords);
		mvprintw(10, 36 + strlen(_("4. Words On Screen:")), "%2d",
		    rules.maxwords);
		if (rules.hightype)
			mvaddstr(11, 31 + strlen(_("   High Score Enabled:")),
			    _("yes"));
		else
			mvaddstr(11, 31 + strlen(_("   High Score Enabled:")),
			    _("no"));
		mvprintw(12, 31 + strlen(_("6. Speed Step:")), "%3d",
		    rules.step);
		mvprintw(13, 31 + strlen(_("7. Speed Range:")), "%2d",
		    rules.minspeed);
		if (rules.maxspeed)
			mvprintw(13, 36 + strlen(_("7. Speed Range:")), "%2d",
			    rules.maxspeed);
		else
			mvaddstr(13, 37 + strlen(_("7. Speed Range:")),
			    _("0 (unlimited)"));
		if (rules.smooth)
			mvaddstr(14, 31 + strlen(_("8. Smoothness:")),
			    _("yes"));
		else
			mvaddstr(14, 31 + strlen(_("8. Smoothness:")), _("no"));
		xcolor_set(5);
		mvaddstr(17, 30, _("Choose: "));

		switch (wheretogo = getch()) {
		case '1':
			chooseruleset();
			break;
		case '2':
			if ((tmp = getnum(_("Misses Limit:"), 0, 100)))
				rules.misses = tmp;
			break;
		case '3':
			if ((tmp = getnum(_("Min Length:"), 0, 20)))
				rules.minlen = tmp;
			if ((tmp = getnum(_("Max Length:"), 0, 20)))
				rules.maxlen = tmp;
			if (rules.minlen > rules.maxlen)
				rules.maxlen = rules.minlen;
			break;
		case '4':
			if ((tmp = getnum(_("Min Words:"), 0, 23)))
				rules.minwords = tmp;
			if ((tmp = getnum(_("Max Words:"), 0, 23)))
				rules.maxwords = tmp;
			if (rules.minwords > rules.maxwords)
				rules.maxwords = rules.minwords;
			break;
		case '6':
			if ((tmp = getnum(_("Speed Step:"), 0, 1000)))
				rules.step = tmp;
			break;
		case '7':
			if ((tmp = getnum(_("Min Speed:"), 0, 100)))
				rules.minspeed = tmp;
			rules.maxspeed = getnum(_("Max Speed:"), 0, 100);
			if (rules.minspeed > rules.maxspeed)
				rules.maxspeed = 0;
			break;
		case '8':
			rules.smooth = dochange(rules.smooth);
			rules.hightype = 0;
			break;
		default:
			break;
		}
	} while (wheretogo != '9');
}

/* Shows high scores. */
void
showhighscores(void)
{
	choosewordfile(1);
}

/* Draws credits onto screen. */
void
tellstory(void)
{
	drawscreen();

	mvaddstr( 3, 2, "Typespeed ");
	addstr(TVERSION);
	mvaddstr( 5, 2,
	    _("Typespeed is made by  Jani Ollikainen  <bestis@iki.fi>"));
	mvaddstr( 6, 2,
	    _("                    & Jaakko Manelius  <jman@iki.fi>"));
	mvaddstr( 7, 2,
	    _("Current maintainer is Tobias Stoeckmann  <tobias@bugol.de>"));

	mvaddstr( 9, 2,
	    _("Typespeed's idea is ripped from ztspeed (a dos game made"));
	mvaddstr(10, 2,
	    _("by Zorlim). Typespeed doesn't give scores that can be"));
	mvaddstr(11, 2,
	    _("compared with ztspeed anymore. We wanted to use our way."));
	mvaddstr(13, 2,
	    _("Idea of the game should be clear to anyone, just type and"));
	mvaddstr(14, 2,
	    _("type it fast, or be a lewser."));
	mvaddstr(16, 2,
	    _("Bugs/Ideas/Comments to <tobias@bugol.de>"));

	pressanykey(18, 2);
}

