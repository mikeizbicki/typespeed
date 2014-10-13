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
 * typespeed.c - the main code
 */

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_SYS_PARAM_H
	#include <sys/param.h>
#endif /* HAVE_SYS_PARAM_H */

#ifdef HAVE_SYS_SOCKET_H
	#include <sys/socket.h>
#endif /* HAVE_SYS_SOCKET_H */

#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#ifdef HAVE_BITS_POSIX1_LIM_H
	#include <bits/posix1_lim.h>
#endif /* HAVE_BITS_POSIX1_LIM_H */

#include <ctype.h>
#include <curses.h>
#include <errno.h>

#ifdef HAVE_FCNTL_H
	#include <fcntl.h>
#endif /* HAVE_FCNTL_H */

#include <getopt.h>

#ifdef HAVE_LOCALE_H
	#include <locale.h>
#endif /* HAVE_LOCALE_H */

#include <signal.h>
#include <stdio.h>

#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
	#include <string.h>
#endif /* HAVE_STRING_H */

#include <time.h>

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "gettext.h"
#include "pathnames.h"
#include "typespeed.h"

#define _(string)	gettext(string)

#define DEFAULT_CHEAT		0
#define DEFAULT_COLORS		1
#define DEFAULT_PORT		6025

extern int		addscore(char *, struct stats *);
extern int		choosewordfile(int);
extern void		closenetwork(void);
extern unsigned long	cstrl(char *);
extern void		drawmenu(void);
extern void		drawscreen(void);
extern void		drawstatus(unsigned);
extern void		endcursestuff(void);
extern void		freewords(void);
extern void		initcursestuff(void);
extern int		initnetwork(char *, int);
extern void		initstatus(char *);
extern unsigned short	level(int);
extern int		loadscores(char *);
extern void		makescorefiles(char *);
extern void		multipmenu(void);
extern int		netrecv(int, int, int, int, char *, size_t);
extern int		netsend(char *);
extern int		netswapscore(struct stats *, struct stats *);
extern void		optionmenu(void);
extern void		pressanykey(int, int);
extern int		r(int);
extern void		readconfig(void);
extern void		rulesmenu(void);
extern void		showhighscores(void);
extern void		setnoblock(void);
extern void		setruleset(char *);
extern void		tellstory(void);
extern clock_t		timenow(void);
extern int		typorankkaus(float);
extern void		xcolor_set(short);
extern void		xerr(int, const char *, ...);
extern void		xerrx(int, const char *, ...);
extern void		xstrncpy(char *, char *, size_t);

int			addnewword(char *);
void			defrule(void);

#ifdef WIN32
void			win32_sighandler(int);
#endif /* WIN32 */

#ifndef TEST
static void		clearword(int, int, size_t);
static void		cwords(void);
static void		movewords(int *);
static void		parseinput(int *, char *, clock_t *, unsigned *, int *, int*);
static int		play(void);
static void		usage(void);
#endif /* TEST */

/* globals */
char *rankki[11] =
{"None", "Beginner", "Learner", "NoGood", "Average",
 "Good", "VeryGood", "Pro", "3l33t", "*(GOD)*", "Computer"};

char *typorank[12] =
{"None", "Alien", "Secretary", "Human", "IT Person", "Handicap",
 "Monkey", "Pencil", "T-Bone", "E-Typo", "TypOmatiC", "TypoKing"};

FILE		*netlogfile;
struct stats	 best;
int		 hfd;
int		 misses;
struct stats	 other;
char		*progname;
float		 rate;
char		*usedwordfile;
int		 wordcount = 0;
int		 wordpos[22];
char		 wordstring[22][20];

struct option options[] = {
	{"cheat", no_argument, &opt.cheat, 1},
	{"client", required_argument, NULL, 'o'},
	{"help", no_argument, NULL, 'h'},
	{"nocolors", no_argument, &opt.usecolors, 0},
	{"netlog", required_argument, NULL, 'n'},
	{"port", required_argument, NULL, 'p'},
	{"seed", required_argument, NULL, 'r'},
	{"server", no_argument, &opt.net, H2H},
	{0, 0, 0, 0}
};

/*
 * Insert a new word into wordstring and wordpos, i.e. one more word
 * on the screen.
 *
 * If newword points to a string, newword will be inserted.
 * If newword is NULL, a random word out of word will be inserted.
 * In both cases, duplicates are not allowed. Should newword point
 * to duplicated string or string that cannot be typed with current
 * locale, no string will be inserted.
 *
 * Returns TRUE on success or FALSE on failure.
 *
 * XXX: Can result in a veeery long loop when there are many words on
 *      field and only a few words in word list (no duplicates allowed).
 */
int
addnewword(char *newword)
{
	int count, dup, slot;
	int freeslot[22];
	size_t i;
	char *myword, *p;

	if (newword != NULL) {
		if (strlen(newword) > (size_t)rules.maxlen)
			return FALSE;
		for (p = newword; *p != '\0'; p++)
			if (!isprint(*p) || isspace(*p))
				return FALSE;
	}

	/* take a random free slot */
	for (count = 0, i = 0; i < 22; i++)
		if (wordpos[i] == -2)
			freeslot[count++] = i;

	if (!count || count < 23 - rules.maxwords)
		/* the player has enough trouble... */
		return FALSE;

	slot = freeslot[r(count)];
	myword = (newword == NULL) ? words.word[r(words.n)] : newword;

	do {
		for (dup = 0, i = 0; i < 22; i++)
			if (!strcmp(wordstring[i], myword)) {
				if (newword != NULL)
					return FALSE;
				dup = 1;
				myword = words.word[r(words.n)];
				break;
			}
	} while (dup);

	xstrncpy(wordstring[slot], myword, sizeof(wordstring[slot]) - 1);
	wordpos[slot] = -1;

	return TRUE;
}

#ifndef TEST
/* Writes "length" blanks at position x y. */
static void
clearword(int y, int x, size_t length)
{
	move(y, x);
	while (length--)
		addch(' ');
}
#endif /* TEST */

#ifndef TEST
/*
 * Sets rate according to current length of all visible words.
 * If words with too few chars are on screen, throw in another word.
 */
static void
cwords(void)
{
	int i, wc;
	unsigned long length;

	for (wc = 0, length = 0, i = 0; i < 22; i++)
		if (wordpos[i] > -2) {
			wc++;
			length += strlen(wordstring[i]) + 1;
		}

	if (length < now.score / 4 + 1 || wc < rules.minwords)
		addnewword(NULL);

	if (rules.smooth)
		rate = (float)now.score / rules.step + rules.minspeed;
	else
		rate = now.score / rules.step + rules.minspeed;

	if (rules.maxspeed && rate > rules.maxspeed)
		rate = rules.maxspeed;
}
#endif /* TEST */

/*
 * Resets rules to typespeed's default.
 */
void
defrule(void)
{
	rules.misses = 10;
	rules.minlen = 1;
	rules.maxlen = 19;
	rules.minwords = 1;
	rules.maxwords = 22;
	rules.hightype = 1;
	rules.minscore = 0;
	rules.minspeed = 3;
	rules.maxspeed = 0;
	rules.step = 175;
	rules.smooth = 1;
	xstrncpy(rules.fname, "default", sizeof(rules.fname) - 1);
	xstrncpy(rules.name, _("default"), sizeof(rules.name) - 1);
}

#ifdef WIN32
void
sleep(clock_t wait)
{
	clock_t goal;

	goal = wait * 1000 + clock();
	while (goal > clock())
		;
}
#endif /* WIN32 */

#ifndef TEST
int
main(int argc, char **argv)
{
	int i, wheretogo;
#ifndef WIN32
	gid_t mygid;
#endif /* WIN32 */
	char serv[MAXHOSTNAMELEN];
	unsigned long val;

	if ((progname = strrchr(argv[0], '/')) == NULL)
		progname = argv[0];
	else
		progname++;

	/* just open high score file while being setgid games */
	if ((hfd = open(HIGHFILE, O_RDWR, 0)) == -1)
		xerr(1, "main: open: %s", HIGHFILE);

#ifndef WIN32
	/* drop privileges */
	mygid = getgid();
#if defined(HAVE_SETRESGID)
	if (setresgid(mygid, mygid, mygid) == -1) {
		fputs("Cannot drop privilege!\n", stderr);
		exit(1);
	}
#elif defined(HAVE_RESREGID)
	if (setregid(mygid, mygid) == -1) {
		fputs("Cannot drop privilege!\n", stderr);
		exit(1);
	}
#else
	if (setegid(mygid) == -1) {
		fputs("Cannot drop privilege!\n", stderr);
		exit(1);
	}
	if (setgid(mygid) == -1) {
		fputs("Cannot drop privilege!\n", stderr);
		exit(1);
	}
#endif /* HAVE_SETRESGID */
#endif /* WIN32 */

	/* check file descriptors for consistency */
	if (hfd == STDIN_FILENO || hfd == STDOUT_FILENO ||
	    hfd == STDERR_FILENO)
		exit(1);
	if (!isatty(STDIN_FILENO) || !isatty(STDOUT_FILENO) ||
	    !isatty(STDERR_FILENO))
		xerrx(1, "not fully connected to a terminal");

	if (setlocale(LC_ALL, "") == NULL)
		xerrx(1, "main: setlocale");

	if (bindtextdomain(PACKAGE, LOCALEDIR) == NULL)
		xerr(1, "main: bindtextdomain");
	if (textdomain(PACKAGE) == NULL)
		xerr(1, "main: textdomain");

#ifdef WIN32
	/* properly exit if someone presses close button */
	(void)signal(SIGINT, &win32_sighandler);
#else
	/* ignore SIGPIPE (for network code) */
	if (signal(SIGPIPE, SIG_IGN) == SIG_ERR)
		xerrx(1, "main: cannot drop SIGPIPE");
#endif /* WIN32 */

	opt.cheat = DEFAULT_CHEAT;
	opt.net = 0;
	opt.port = DEFAULT_PORT;
	opt.usecolors = DEFAULT_COLORS;
	strncpy(opt.name, "default", sizeof(opt.name));
	opt.name[sizeof(opt.name) - 1] = '\0';

	usedwordfile = NULL;

	defrule();

	memset(&best, 0, sizeof(best));
	memset(&now, 0, sizeof(now));

	readconfig();
	if (strcmp(rules.fname, "default"))
		setruleset(rules.fname);

	/* Prepare status bar, depends on locale */
	initstatus(
	    _("Rank: %------- Score: %--- WPM: %-- CPS: %---- Misses: %-"));

	while ((i = getopt_long(argc, argv, "to:h", options, NULL)) != -1) {
		switch (i) {
		case 0:
			/* flag already set by getopt */
			break;
		case 'n':
			if (netlogfile != NULL)
				if (fclose(netlogfile))
					xerr(1, "main: fclose netlogfile");
			if ((netlogfile = fopen(optarg, "w")) == NULL)
				xerr(1, "main: fopen netlogfile");
			break;
		case 'o':
			opt.net = H2H;
			if (optarg == NULL)
				/* getopt should handle this */
				abort();
			xstrncpy(serv, optarg, sizeof(serv) - 1);
			break;
		case 'p':
			if (optarg == NULL)
				/* getopt should handle this */
				abort();
			if ((!(val = cstrl(optarg)) && errno) ||
			    val > 65535)
				xerrx(1, "Cannot convert %s to port number",
				    optarg);
			if (val < 1025)
				xerrx(1, "Do not use privileged port!");
			opt.port = val;
			break;
		case 'r':
			if (optarg == NULL)
				/* getopt should handle this */
				abort();
			if ((!(val = cstrl(optarg)) && errno) ||
			    val > UINT_MAX)
				xerrx(1, "Cannot convert %s to random seed",
				    optarg);
			opt.seed = val;
			break;
		case 't':
			opt.net = NET;
			if (optarg == NULL)
				/* getopt should handle this */
				abort();
			xstrncpy(serv, optarg, sizeof(serv) - 1);
			break;
		case ':':
		case '?':
			/* getopt already printed error */
		case 'h':
		default:
			usage();
			/* NOTREACHED */
			break;
		}
	}
	argc -= optind;
	argv += optind;

	if (argc)
		usage();
		/* NOTREACHED */

	if (opt.net) {
		if (initnetwork(serv, 0)) {
			fflush(stdout);
			xerrx(1, "main: initnetwork");
		}
		fflush(stdout);
		setnoblock();
	}

	sleep(1);
	initcursestuff();

	if (opt.net == H2H)
		play();

    play();


	endcursestuff();

	puts("\033[H\033[J");
	fputc('\n', stdout);
	return 0;
}
#endif /* TEST */

#ifndef TEST
/*
 * Move words across the screen.
 * Removes them if they reach the right side and returns the amount of
 * words which left the screen.
 */
static void
movewords(int *misses)
{
	int buf[4][22]; 
	int count[4];
	int i, j, length;

	memset(count, 0, sizeof(count));

	xcolor_set(0);
	for (i = 0; i < 22; i++) {
		if (wordpos[i] == -2)
			continue;

		length = (int)strlen(wordstring[i]);

		if (wordpos[i] > (79 - length)) {
			clearword(i, wordpos[i], length);
			buf[3][count[3]++] = i;
			wordpos[i] = -2;
			continue;
		}

		if (wordpos[i] > -1)
			mvaddch(i, wordpos[i], ' ');

		if (wordpos[i] > (65 - length))
			buf[2][count[2]++] = i;
		else if (wordpos[i] > (50 - length))
			buf[1][count[1]++] = i;
		else
			buf[0][count[0]++] = i;
	}

	*misses += count[3];

	/*
	 * Get words that are missed back in buffer, maybe the player
	 * was about to type them.
	 */
	if (*misses >= rules.misses) {
		for (i = 0; i < count[3]; i++)
			wordpos[buf[3][i]] = 0;
		return;
	}

	for (j = 0; j < 3; j++) {
		if (!count[j])
			continue;

		switch (j) {
		case 0:
			xcolor_set(1);
			break;
		case 1:
			xcolor_set(7);
			break;
		case 2:
			xcolor_set(3);
			break;
		default:
			break;
		}

		for (i = 0; i < count[j]; i++)
			mvaddstr(buf[j][i], ++wordpos[buf[j][i]],
			    wordstring[buf[j][i]]);
	}
}
#endif /* TEST */

#ifndef TEST
/*
 * Parses user input during game.
 */
static void
parseinput(int *escend, char *input, clock_t *starttime, unsigned *inputpos,
    int *nappaykset, int *wrngret)
{
	int i, nappi, foundword;
	clock_t pausetime;

	if ((nappi = getch()) == ERR)
		return;

	switch (tolower(nappi)) {
	case 8:
	case 127:
	case KEY_BACKSPACE:
		if (*inputpos) {
			mvaddch(23, 1 + *inputpos, ' ');
			input[--(*inputpos)] = '\0';
		}
		break;
	case 27: /* ESC */
		*escend = 1;
		break;
	case KEY_UP:
		if (opt.net) {
			*escend = 1;
			break;
		}

		pausetime = timenow();
		mvaddstr(23, 2, _("       PAUSED      "));
		move(23, 2);
		while (getch() == ERR)
			;
		*starttime += timenow() - pausetime;
		mvaddstr(23, 2, "                   ");
		flushinp();
		/* FALLTHROUGH */
	case 21: /* ^U */
		if (*inputpos) {
			clearword(23, 2, *inputpos);
			*inputpos = 0;
			input[0] = '\0';
		}
		break;
	case 32: /* SPACE */
	case 10: /* ENTER */
		foundword = 0;
		for (i = 0; i < 22; i++) {
			if (strcmp(input, wordstring[i]))
				continue;
			foundword = 1;
			now.wordswritten++;
			now.score += *inputpos;
			clearword(i, wordpos[i], strlen(wordstring[i]));
			wordpos[i] = -2;
			switch (opt.net) {
			case H2H:
				if (r(2))
					break;
				/* FALLTHROUGH */
			case NET:
				if (netsend(wordstring[i]))
					*escend = 2;
				break;
			default:
				break;
			}
			xstrncpy(wordstring[i], " ", sizeof(wordstring[i]) - 1);
		}
		if (!foundword) {
			(*nappaykset)++;
			(*wrngret)++;
		}
		clearword(23, 2, 19);
		*inputpos = 0;
		input[0] = '\0';
		break;
	case 11: /* ^K */
		if ((size_t)*inputpos < strlen(input)) {
			input[*inputpos] = '\0';
			clearword(23, 2 + *inputpos, 19 - *inputpos);
		}
		break;
	case KEY_RIGHT:
	case 6: /* ^F */
		if (*inputpos < 19 && (size_t)*inputpos < strlen(input))
			(*inputpos)++;
		break;
	case KEY_END:
	case 5: /* ^E */
		*inputpos = strlen(input);
		break;
	case KEY_LEFT:
	case 2: /* ^B */
		if (*inputpos)
			(*inputpos)--;
		break;
	case KEY_HOME:
	case 1: /* ^A */
		*inputpos = 0;
		break;
	default:
		if (nappi > 255)
			break;
		(*nappaykset)++;
		if (*inputpos > 18 || iscntrl(nappi))
			break;
		mvaddch(23, 2 + *inputpos, nappi);
		if (*inputpos == strlen(input))
			input[*inputpos + 1] = '\0';
		input[(*inputpos)++] = nappi;
		if (!opt.cheat)
			break;
		for (i = 0; i < 22; i++) {
			if (strcmp(input, wordstring[i]))
				continue;
			now.score += *inputpos;
			clearword(i, wordpos[i], strlen(wordstring[i]));
			wordpos[i] = -2;
			clearword(23, 2, 19);
			*inputpos = 0;
			input[0]= '\0';
			xstrncpy(wordstring[i], " ", sizeof(wordstring[i]) - 1);
		}
		break;
	}
	move(23, 2 + *inputpos);
}
#endif /* TEST */

#ifndef TEST
/*
 * Contains core game logic.
 * This function runs as long as player hasn't reached main screen again. :)
 */
static int
play(void)
{
	int escend, i, nappaykset, wordcount, wrngret;
	unsigned inputpos, wordlim;
	clock_t oldtimes, starttime;
	char input[20];

	memset(&now, 0, sizeof(now));
	now.sinit = opt.seed ? opt.seed : (uint32_t)time(NULL);

#ifdef WIN32
	srand(now.sinit);
#else
	srandom(now.sinit);
#endif /* WIN32 */

	if (opt.net == H2H)
		defrule();

	if (choosewordfile(0)) {
		if (opt.net == H2H) {
			(void)netsend(" ERROR");
			closenetwork();
		}
		return 1;
	}

	misses = 0;
	escend = inputpos = nappaykset = 0;
	rate = 1;

	wordlim = (rules.maxwords == 1) ? 0 : 1;

	drawscreen();
	halfdelay(1);

	if (opt.net) {
		xcolor_set(2);
		mvaddstr(12, 20,
		    _("Waiting for the other party to join in..."));
		refresh();
		if (netsend(" GAME START") || netrecv(1, 0, 1, 1, NULL, 0)
		    != 5) {
			nocbreak();
			cbreak();
			closenetwork();
			return 1;
		}
		flushinp();
		drawscreen();
	}

	oldtimes = starttime = timenow();

	/* Prevent an (almost) impossible division by zero later on */
	while (starttime == timenow())
		;

	input[0] = '\0';

	for (i = 0; i < 22; i++) {
		wordstring[i][0] = ' ';
		wordstring[i][1] = '\0';
		wordpos[i] = -2;
	}

	/* move cursor to correct position */
	drawstatus(0);

	while (!escend && misses < rules.misses) {
		wordcount = now.wordswritten;
		wrngret = 0;
		while (timenow() < (oldtimes + (100.0 / rate))) {
			parseinput(&escend, input, &starttime, &inputpos,
			    &nappaykset, &wrngret);
			if (now.wordswritten - wordcount > wordlim &&
			    wrngret > 10) {
				escend = 1;
				rules.hightype = 0;
				memset(&now, 0, sizeof(now));
				nappaykset = 0;
			}
		}
		oldtimes = timenow();
		now.duration = oldtimes - starttime;

		movewords(&misses);

		/*
		 * Be nice and check if a player was to typo something while
		 * the last word slipped away... Every other situation is a
		 * typo though.
		 */
		if (misses >= rules.misses)
			for (i = 0; i < 22; i++)
				if (wordpos[i] != -2 &&
				    !strncmp(wordstring[i], input, inputpos)) {
					now.score += inputpos;
					break;
				}

		cwords();

		now.speed = (now.score + now.wordswritten) * 100.0 /
		    now.duration;

		now.wpm = now.speed * 12;

		drawstatus(inputpos);

		/* last, we check if the opponent has new words for us... */
		if (opt.net && !escend) {
			switch(netrecv(0, 0, 1, 1, NULL, 0)) {
			case -1:
			case 2:
				escend = 2;
				break;
			default:
				break;
			}
		}

		refresh();
	}

	nocbreak();
	cbreak();

	if (opt.net && netsend(" GAME END")) {
		closenetwork();
		return -1;
	}

	drawscreen();
	xcolor_set(5);
	mvaddstr(12, 30, _("GAME OVER!"));
	refresh();

	now.duration = oldtimes - starttime;
	now.tcount = nappaykset;
	now.level = level(now.score);

	if (now.tcount)
		now.ratio = (1 - (float)(now.score + now.wordswritten) /
		(now.tcount + now.wordswritten)) * 100;

	if (now.score >= best.score)
		memcpy((void *)&best, (void *)&now, sizeof(best));

	if (opt.net == H2H) {
		mvaddstr(14, 23, _("Retrieving opponent's high score ..."));
		refresh();
		if (netswapscore(&now, &other))
			memset(&other, 0, sizeof(other));
	}

	sleep(3);
	drawscreen();
	xcolor_set(5);
	refresh();

	clearword(12, 30, strlen(_("GAME OVER!")));

	mvaddstr(3, 20, "Typespeed ");
	addstr(TVERSION);

	mvaddstr(5, 20, _("You achieved:"));

	mvaddstr(7, 20, _("Rank:"));
	mvprintw(8, 20, _("Score:\t\t%d"), now.score);
	mvprintw(9, 20, _("WPM:\t\t%lu"), now.wpm);
	mvprintw(10, 20, _("CPS:\t\t%2.3f"), now.speed);
	mvprintw(11, 20, _("Typo ratio:\t\t%2.1f%%"), now.ratio);
	mvaddstr(12, 20, _("Typorank:"));

	/* these functions change color */
	mvaddstr(7, 40, rankki[level(now.score)]);
	mvaddstr(12, 40, typorank[now.tcount ? typorankkaus(now.ratio) : 0]);

	if (opt.net == H2H) {
		xcolor_set(5);
		if (other.name[0] != '\0' && strcmp(other.name, "default"))
			mvprintw(5, 60, "%s:", other.name);
		else
			mvaddstr(5, 60, _("Opponent:"));
		mvprintw(8, 60, "%d", other.score);
		mvprintw(9, 60, "%lu", other.wpm);
		mvprintw(10, 60, "%2.3f", other.speed);
		if (other.tcount)
			other.ratio = (1 - (float)(other.score +
			    other.wordswritten) / (other.tcount +
			    other.wordswritten)) * 100;
		else
			other.ratio = 0;
		mvprintw(11, 60, _("%2.1f%%"), other.ratio);

		/* these functions change color */
		mvaddstr(7, 60, rankki[level(other.score)]);
		mvaddstr(12, 60, typorank[other.tcount ?
		    typorankkaus(other.ratio) : 0]);
	}

	xcolor_set(2);
	if (opt.net == H2H)
		mvaddstr(17, 15, _("NETWORK MODE - YOU CANNOT GET TO TOP10"));
	else if (!rules.hightype)
		mvaddstr(17, 15,
		    _("RANKING DISABLED - YOU CANNOT GET TO TOP10"));

	pressanykey(15, 20);
	if ((!opt.net || opt.net == NET) && now.score > rules.minscore
	    && rules.hightype) {
		addscore(usedwordfile, &now);
		loadscores(usedwordfile);
                execl("/usr/local/bin/Scorescreen", (char*)0);
	}
	free(usedwordfile);
	usedwordfile = NULL;
	freewords();

	if (opt.net == H2H)
		closenetwork();

	return 0;
}
#endif /* TEST */

#ifndef TEST
/* Prints usage information on stderr and exits with return value 1. */
static void
usage()
{
	xerrx(1 , _("\n\
command line options:\n\
  --cheat       = multimedia cheat(?)\n\
  --client=addr = ip address of server\n\
  --netlog=file = log network traffic into file\n\
  --nocolors    = do not use colors\n\
  --port=port   = port number in the network play\n\
  --server      = start typespeed in server mode\n\
  --help        = this help!\n\
\n\
usage: %s options\n\
\n\
typespeed %s - http://tobias.eyedacor.org/typespeed/\n"),
	    progname, TVERSION);
}
#endif /* TEST */

#ifdef WIN32
void
win32_sighandler(int sig)
{
	exit(0);
}
#endif /* WIN32 */

