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
 * file.c - file access functions
 */

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_SYS_FILE_H
	#include <sys/file.h>
#endif /* HAVE_SYS_FILE_H */

#ifdef HAVE_SYS_PARAM_H
	#include <sys/param.h>
#endif /* HAVE_SYS_PARAM_H */

#ifdef HAVE_SYS_STAT_H
	#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#ifdef HAVE_BITS_POSIX1_LIM_H
	#include <bits/posix1_lim.h>
#endif /* HAVE_BITS_POSIX1_LIM_H */

#include <ctype.h>
#include <curses.h>

#ifdef HAVE_DIRENT_H
	#include <dirent.h>
#endif /* HAVE_DIRENT_H */

#include <errno.h>

#ifdef HAVE_FCNTL_H
	#include <fcntl.h>
#endif /* HAVE_FCNTL_H */

#include <stdint.h>
#include <stdio.h>

#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
	#include <string.h>
#endif /* HAVE_STRING_H */

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "gettext.h"
#include "pathnames.h"
#include "typespeed.h"

#define _(string)	gettext(string)
#define WORDFILE_MAX	100

struct hentry {
	uint32_t count;
	uint32_t tcount;
	uint32_t wordswritten;
	char name[41];
	char mod[FILENAME_MAX * 2 + 1];
	char wordlist[FILENAME_MAX * 2 + 1];
	clock_t duration;
	uint32_t sinit;
};

extern unsigned long	 cstrl(char *);
extern void		 defrule(void);
extern void		 drawscreen(void);
extern int		 fileselmenu(int, struct finfo *, const char *);
extern unsigned short	 level(int);
extern void		 liima_mvgetnstr(int, int, char *, int);
extern int		 netrecv(int, int, int, int, char *, size_t);
extern int		 netsend(char *);
extern int		 netsendscore(struct stats *, char *);
extern void		 nowordlist(void);
extern void		 pressanykey(int, int);
extern int		 typorankkaus(float);
extern void		 xcolor_set(short);
extern void		 xerr(int, const char *, ...);
extern void		 xerrx(int, const char *, ...);
extern void		*xmalloc(size_t);
extern int		 xsnprintf(char *, size_t, const char *, ...);
extern char		*xstrdup(char *);
extern void		 xstrncpy(char *, char *, size_t);

int			 addscore(char *, struct stats *);
int			 chooseruleset(void);
int			 choosewordfile(int);
int			 compar(const void *, const void *);
void			 freewords(void);
static struct hentry	*getentry(char *);
static int		 getfinfo(struct finfo *, int n, char *);
static int		 getnum(char *);
int			 hcompar(const void *, const void *);
int			 loadscores(char *);
static int		 parseline(char *, char **, char **);
void			 readconfig(void);
static void		 readfile(char *, int);
static void		 setoptions(char *, char *, int);

#ifdef TEST
char			*escstr(char *);
int			 loadwords(char *);
char			*unescstr(char *);
#else
static char		*escstr(char *);
static int		 loadwords(char *);
static char		*unescstr(char *);
#endif /* TEST */

extern int	 hfd;
extern char	*rankki[11];
extern char	*typorank[12];
extern char	*usedwordfile;

char ruledir[MAXPATHLEN];
char worddir[MAXPATHLEN];

/*
 * Asks player about his/her name. The entered name must be
 * changed so tabs are escaped. During write process, the
 * high score file will be locked.
 *
 * Returns 0 on success and 1 on failure.
 */
int
addscore(char *wordlist, struct stats *stat)
{
	FILE *highf;
	int fd;
	char *rname, *uname;
	char name[100];

	/* Ask player until a name has been entered. */
	do {
		drawscreen();
        /*mvaddstr(3, 2, _("Enter your name:"));*/
        /*liima_mvgetnstr(3, 3 + strlen(_("Enter your name:")), name,*/
        /*sizeof(name) - 1);*/
        /*name[sizeof(name) - 1] = '\0';*/
        getlogin_r(name,sizeof(name));
	} while (name[0] == '\0' || name[0] == ' ');

	if (opt.net == NET)
		netsendscore(stat, name);
	else {
		rname = escstr(rules.fname);
		uname = escstr(name);

#ifndef WIN32
		(void)flock(hfd, LOCK_EX);
#endif /* WIN32 */

		if ((fd = dup(hfd)) == -1)
			return 1;

		if ((highf = fdopen(fd, "r+")) == NULL)
			return 1;

		fseek(highf, 0, SEEK_END);
		fprintf(highf, "%u\t%u\t%u\t%s\t%s\t%s\t%u\t%u\n",
		    stat->score, stat->tcount, stat->wordswritten, uname,
		    wordlist, rname, (uint32_t)stat->duration, stat->sinit);

		if (fclose(highf) == EOF)
			return 1;

#ifndef WIN32
		(void)flock(hfd, LOCK_UN);
#endif /* WIN32 */

		free(rname);
		free(uname);
	}

	return 0;
}

/*
 * This function asks player about rule set of choice. All rule set
 * variables will be set.
 *
 * Returns ...
 * 0 on success
 * 1 if no rule set available
 * -1 if path to rule set is too long
 */
int
chooseruleset(void)
{
	int k;
	char fullpath[MAXPATHLEN];
	struct finfo namelist[WORDFILE_MAX];

	drawscreen();

	/* Default rule set must be selectable. */
	xstrncpy(namelist[0].descr, _("default"),
	    sizeof(namelist[0].descr) - 1);
	xstrncpy(namelist[0].name, "default", sizeof(namelist[0].name) - 1);

	k = getfinfo(&(namelist[1]), WORDFILE_MAX - 1, "rule.");
	if (k++ < 1) {
		mvaddstr(1, 5, _("No rule sets found..."));
		pressanykey(3, 5);
		return 1;
	}

	qsort((void *)(namelist + 1), k - 1, sizeof(namelist[0]), compar);

	if (!(k = fileselmenu(k, namelist, _("rule set")))) {
		defrule();
		return 0;
	}
	if (k == -1)
		return 0;

	if (opt.net == NET) {
		if (xsnprintf(fullpath, sizeof(fullpath), "%s",
		    namelist[k].name))
			return -1;
	} else {
		if (xsnprintf(fullpath, sizeof(fullpath), "%s/%s", ruledir,
		    namelist[k].name))
			return -1;
	}

	readfile(fullpath, 0);
	xstrncpy(rules.fname, namelist[k].name, sizeof(rules.fname) - 1);
	xstrncpy(rules.name, namelist[k].descr, sizeof(rules.name) - 1);

	return 0;
}

/*
 * A huge function that asks player about word list.
 * In this menu you can select found word lists for further
 * processing. The processing of the word list depends on
 * "operation":
 *
 * 0: loads words out of word list
 * 1: prints high scores for word list (and current rule set)
 *
 * Returns 0 on success or 1 on failure.
 */
int
choosewordfile(int operation)
{
	int a, k;
	const char *errstr;
	struct finfo namelist[WORDFILE_MAX];

	k = getfinfo(namelist, WORDFILE_MAX, "words.");
	if (k < 1) {
		drawscreen();
		mvaddstr(1, 5, _("No word lists found..."));
		pressanykey(3, 5);
		return 1;
		/* NOTREACHED */
	}

	qsort((void *)namelist, k, sizeof(namelist[0]), compar);

	if ((k = fileselmenu(k, namelist, _("word list"))) == -1)
		return 1;

	switch (operation) {
	case 0:
		a = loadwords(namelist[k].name);
		break;
	case 1:
		loadscores(namelist[k].name);
		return 0;
		/* NOTREACHED */
		break;
	default:
		a = -1;
		break;
	}

	switch (a) {
	case 0: /* success */
		if (usedwordfile != NULL)
			free(usedwordfile);
		usedwordfile = xstrdup(namelist[k].name);
		return 0;
		/* NOTREACHED */
		break;
	case 1: /* xsnprintf */
		errstr = _("Path to file is too long.");
		break;
	case 2: /* fopen - errno */
		errstr = strerror(errno);
		break;
	case 3: /* loadwords - not enough words */
		errstr = _("Not enough words in word list for current settings.");
		break;
	case 5: /* resetscorefile - error during write */
		errstr = _("Cannot write to high score file.");
		break;
	default:
		errstr = _("Something went wrong. Contact your mother.");
		break;
	}

	xcolor_set(2);
	mvaddstr(3, 5, errstr);
	getch();

	return 1;
}

/* helper function for qsort */
int
compar(const void *p1, const void *p2)
{
	int retval;

	struct finfo *elem1, *elem2;

	elem1 = (struct finfo *)p1;
	elem2 = (struct finfo *)p2;

	if (!(retval = strcmp(elem1->descr, elem2->descr)))
		xerrx(1, "%s/%s and %s/%s have the same description!",
		    worddir, elem1->name, worddir, elem2->name);
	return retval;
}

/*
 * All entries in high score file must be valid. If a player
 * has created a word list with a file name that contains a tab
 * (or rule set with tab), the tab must be escaped.
 *
 * Returns converted string that must be passed to free() later on.
 */
#ifndef TEST
static
#endif /* TEST */
char *
escstr(char *string)
{
	int pos;
	char *new, *p, *retval;

	if (string == NULL)
		return NULL;

	if ((new = calloc(2, strlen(string) + 1)) == NULL)
		xerr(1, "escstr: calloc");

	for (pos = 0, p = string; *p != '\0'; p++) {
		if (*p == '\\' || *p == '\t')
			new[pos++] = '\\';
		new[pos++] = *p;
	}
	new[pos] = '\0';

	retval = xstrdup(new);
	free(new);

	return retval;
}

/* Frees all allocated memory in word. */
void
freewords(void)
{
	if (words.bulk != NULL) {
		free(words.bulk);
		words.bulk = NULL;
	}
	if (words.word != NULL) {
		free(words.word);
		words.word = NULL;
	}
	words.n = words.max = 0;
}

/*
 * This function converts string "line" into struct hentry. The converted
 * struct will be returned and must be passed to free() later on.
 *
 * Returns converted struct.
 */
static struct hentry *
getentry(char *line)
{
	int i, pos;
	char *p, *q, *t;
	struct hentry *entry;

	entry = xmalloc(sizeof(struct hentry));
	p = line;

	for (pos = 0; pos < 7; pos++) {
		/* do not allow empty field */
		if ((q = strchr(p, '\t')) == NULL || q == p)
			xerrx(1, "bad entry in high score detected");

		for (i = 1; (q - i) >= p && *(q - i) == '\\'; i++)
			;

		if (!(i % 2)) {
			p = q + 1;
			continue;
		}
		*q = '\0';

		switch(pos) {
		case 0: /* count */
			if (!(entry->count = cstrl(line)) && errno)
				xerrx(1, "count is invalid: %s", line);
			break;
		case 1: /* tcount */
			if (!(entry->tcount = cstrl(line)) && errno)
				xerrx(1, "tcount is invalid: %s", line);
			break;
		case 2: /* wordswritten */
			if (!(entry->wordswritten = cstrl(line)) && errno)
				xerrx(1, "wordswritten is invalid: %s", line);
			break;
		case 3: /* name */
			while (*line == ' ')
				line++;
			t = unescstr(line);
			if (xsnprintf(entry->name, sizeof(entry->name),
			    "%s", t))
				xerrx(1, "name is invalid: %s", t);
			free(t);
			break;
		case 4: /* word list */
			while (*line == ' ')
				line++;
			t = unescstr(line);
			if (xsnprintf(entry->wordlist, sizeof(entry->wordlist),
			    "%s", t))
				xerrx(1, "word list is invalid: %s", t);
			free(t);
			break;
		case 5: /* mod */
			while (*line == ' ')
				line++;
			t = unescstr(line);
			if (xsnprintf(entry->mod, sizeof(entry->mod), "%s", t))
				xerrx(1, "game rule is invalid: %s", t);
			free(t);
			break;
		case 6: /* duration */
			if (!(entry->duration = cstrl(line)) && errno)
				xerrx(1, "duration is invalid: %s", line);
			break;
		default:
			xerrx(1, "too many fields in high score file");
			/* NOTREACHED */
			break;
		}
		line = p = q + 1;
	}
	/* sinit */
	if (!(entry->sinit = cstrl(line)) && errno)
		xerrx(1, "sinit is invalid: %s", line);

	return entry;
}

/*
 * getfinfo is used to gather specific file information. Possible
 * files are game rules and word lists - depending on "prefix".
 * namelist will be filled with all available options.
 *
 * Returns number of game rules/word lists found or -1 on error.
 */
static int
getfinfo(struct finfo *namelist, int n, char *prefix)
{
	DIR *dirp;
	FILE *fileinfo;
	int i, k, op;
	char *p, *path, *q;
	char buf[1024], info[61], wordpath[MAXPATHLEN];
	struct dirent *dp;

	if (!strcmp(prefix, "rule.")) {
		op = 0;
		path = ruledir;
	} else if (!strcmp(prefix, "words.")) {
		op = 1;
		path = worddir;
	} else
		return -1;

	k = 0;

	if (opt.net == NET) {
		switch(op) {
		case 0:
			netsend(" RULESETS GET");
			if (netrecv(1, 1, 1, 1, buf, sizeof(buf)) != 10)
				return -1;
			break;
		case 1:
			netsend(" WORDLISTS GET");
			if (netrecv(1, 1, 1, 1, buf, sizeof(buf)) != 10)
				return -1;
			break;
		default:
			return -1;
			/* NOTREACHED */
			break;
		}
		p = q = buf + sizeof(" FILELIST ") - 1;

		while (*p >= '0' && *p <= '9')
			p++;
		if (*p != '\0')
			return -1;

		i = strtol(q, NULL, 10);
		if (n < i)
			i = n;

		for (k = 0; k < i; k++) {
			if (netrecv(1, 1, 1, 0, info, sizeof(info)) == -1)
				return -1;
			if ((p = strchr(info, '\n')) != NULL)
				*p = '\0';
			xstrncpy(namelist[k].descr, info,
			    sizeof(namelist[k].descr) - 1);
			/* XXX */
			xstrncpy(namelist[k].name, info,
			    sizeof(namelist[k].name) - 1);
		}
	} else {
		if ((dirp = opendir(path)) == NULL)
			return -1;

		while ((dp = readdir(dirp)) != NULL && k < n) {
			if (strncmp(dp->d_name, prefix, strlen(prefix)))
				continue;
			if (xsnprintf(wordpath, sizeof(wordpath), "%s/%s",
			    path, dp->d_name))
				continue;
			if ((fileinfo = fopen(wordpath, "r")) == NULL)
				continue;
			if (fgets(info, sizeof(info), fileinfo) != NULL) {
				if ((p = strchr(info, '\n')) != NULL)
					*p = '\0';
				if ((p = strchr(info, '\r')) != NULL)
					*p = '\0';
				p = info;
				if (!op) {
					if (info[0] != '#')
						goto fileclose;
					p++;
				}
				xstrncpy(namelist[k].descr, p,
				    sizeof(namelist[k].descr) - 1);
				xstrncpy(namelist[k].name, dp->d_name,
				    sizeof(namelist[k].name) - 1);
				k++;
			}
fileclose:
			if (fclose(fileinfo) == EOF)
				xerr(1, "getfinfo: fclose: %s", wordpath);
		}
		(void)closedir(dirp);
	}

	return k;
}

static int
getnum(char *p)
{
	char *start;
	int i;

	start = p;

	while (*p >= '0' && *p <= '9')
		p++;
	if (*p != '\0')
		return -1;

	i = strtol(start, NULL, 10);
	if (i < 0)
		return -1;
	return i;
}

/* helper function for qsort */
int
hcompar(const void *entry1, const void *entry2)
{
	int i;
	struct hentry **en1, **en2;
	unsigned long val1, val2;
	float rat1, rat2;

	en1 = (struct hentry **)entry1;
	en2 = (struct hentry **)entry2;

	for (i = 0; i < 3; i++) {
		switch (opt.order[i]) {
		case 'c':
			if ((*en1)->duration)
				rat1 = (float)((*en1)->count +
				    (*en1)->wordswritten) /
				    (*en1)->duration;
			else
				rat1 = 0.0f;

			if ((*en2)->duration)
				rat2 = (float)((*en2)->count +
				    (*en2)->wordswritten) /
				    (*en2)->duration;
			else
				rat2 = 0.0f;

			if (rat1 < rat2)
				return 1;
			if (rat1 > rat2)
				return -1;
			break;
		case 's':
			val1 = (*en1)->count;
			val2 = (*en2)->count;
			if (val1 < val2)
				return 1;
			else if (val1 > val2)
				return -1;
			break;
		case 't':
			if (!(*en1)->tcount)
				rat1 = 0.0f;
			else
				rat1 = (1 - (float)((*en1)->count +
				    (*en1)->wordswritten) / ((*en1)->tcount +
				    (*en1)->wordswritten));

			if (!(*en2)->tcount)
				rat2 = 0.0f;
			else
				rat2 = (1 - (float)((*en2)->count +
				    (*en2)->wordswritten) / ((*en2)->tcount +
				    (*en2)->wordswritten));

			if (rat1 > rat2)
				return 1;
			if (rat1 < rat2)
				return -1;
			break;
		default:
			xerrx(1, "%c in highorder is invalid", opt.order[i]);
			/* NOTREACHED */
		}
	}

	return 0;
}

/*
 * Loads high score for "filename", where "filename" contains the name
 * of the word list and gives out a high score list for current rule set.
 *
 * Returns 0 on success and 1 on failure.
 */
int
loadscores(char *filename)
{
	FILE *highfile;
	int exitnow, fd, i, lvl, m, n, rank;
	float ratio, cps;
	char *p;
	char buf[1024], line[2 * FILENAME_MAX + 100];
	struct hentry *entry;
	struct hentry **entries, **new;

	entries = NULL;

	if (opt.net == NET) {
		if (xsnprintf(buf, sizeof(buf), " SCORES %s", filename))
			return 1;
		netsend(buf);

		do {
			i = netrecv(1, 1, 1, 1, buf, sizeof(buf) - 2);
			switch (i) {
			case 2:
			case 11:
				break;
			default:
				return 1;
				/* NOTREACHED */
				break;
			}
		} while (i != 11);

		if ((i = getnum(buf + sizeof(" SCORES ") - 1)) < 0)
			return 1;

		n = 0;
		for (; i > 0; i--) {
			if (netrecv(1, 1, 1, 0, line, sizeof(line)) == -1)
				return 1;

/* XXX only top10 */
			if ((p = strchr(line, '\n')) != NULL)
				*p = '\0';
			entry = getentry(line);
			if (!strcmp(entry->wordlist, filename) &&
			    !strcmp(entry->mod,
			    rules.fname)) {
				if ((new = realloc(entries, (n + 1) *
			    	sizeof(struct hentry *))) == NULL)
					xerr(1, NULL);
				entries = new;
				entries[n++] = entry;
			}
			else
				free(entry);
		}
	} else {
#ifndef WIN32
		(void)flock(hfd, LOCK_EX);
#endif /* WIN32 */

		if ((fd = dup(hfd)) == -1)
			return 1;
		if ((highfile = fdopen(fd, "r+")) == NULL)
			return 1;
		rewind(highfile);

		n = 0;
		while (fgets(line, sizeof(line), highfile) != NULL) {
			if ((p = strchr(line, '\n')) == NULL) {
				if (fclose(highfile) == EOF)
					xerr(1, "loadscores: fclose");
#ifndef WIN32
				(void)flock(hfd, LOCK_UN);
#endif /* WIN32 */
				return 1;
			}
			*p = '\0';
			if ((p = strchr(line, '\r')) != NULL)
				*p = '\0';
			entry = getentry(line);
			if (!strcmp(entry->wordlist, filename) &&
			    !strcmp(entry->mod, rules.fname)) {
				if ((new = realloc(entries, (n + 1) *
			    	sizeof(struct hentry *))) == NULL)
					xerr(1, NULL);
				entries = new;
				entries[n++] = entry;
			}
			else
				free(entry);
		}

		if (fclose(highfile) == EOF)
			xerr(1, "loadscores: fclose");

#ifndef WIN32
		(void)flock(hfd, LOCK_UN);
#endif /* WIN32 */
	}

	qsort(entries, n, sizeof(struct hentry *), hcompar);

	if ((new = realloc(entries, (n + 1) * sizeof(struct hentry *)))
	    == NULL)
		xerr(1, NULL);
	entries = new;
	entries[n] = NULL;

	drawscreen();
	xcolor_set(4);

	if (entries[0] == NULL) {
		mvaddstr(4, 3, _("No High Scores"));
		pressanykey(19, 3);
		free(entries);
		return 0;
	}

	m = 0;
	exitnow = 0;
	do {
		drawscreen();
		xcolor_set(4);
		mvaddstr(0, 35, _("High Score List"));
		mvaddstr(2, 19, _("Use cursor keys to scroll or ESC to leave"));
		mvaddstr(4, 0, _(
"Rank   (score) (name)                   (level)   (cps)  (wpm)  (typoinfos)"
		));

		for (i = 0; entries[i + m] != NULL && i < 17; i++) {
			entry = entries[i + m];
			/* XXX typespeed.c */
			if (!entry->tcount)
				ratio = 0;
			else
				ratio = (1 - (float)(entry->count +
				    entry->wordswritten) / (entry->tcount +
				    entry->wordswritten)) * 100;

			/*
			 * These set colors on their own, so put them
			 * in front to safe expensive color_set call.
			 */
			rank = typorankkaus(ratio);
			if (!entries[i]->count)
				rank = 0;
			mvaddstr(i + 5, 71, typorank[rank]);

			lvl = level(entry->count);
			if (lvl >= 0 && lvl < 11)
				mvaddstr(i + 5, 40, rankki[lvl]);

			xcolor_set(5);
			mvprintw(i + 5, 2, "%2d.", i + m + 1);
			mvprintw(i + 5, 7, "%6lu", entry->count);
			mvaddstr(i + 5, 15, entry->name);

			if (entry->duration) {
				cps = (float)(entry->count +
				    entry->wordswritten) /
				    entry->duration * 100.0f;

				mvprintw(i + 5, 50, "%2.3f", cps);
				mvprintw(i + 5, 58, "%3lu", (uint32_t)(12.0f *
				    cps));
			} else {
				mvprintw(i + 5, 50, "%2.3f", 0.0f);
				mvprintw(i + 5, 58, "%3lu", 0);
			}

			mvprintw(i + 5, 64, "%2.2f%%", ratio);

			move(23, 2);
		}

		switch(getch()) {
		case KEY_DOWN:
		case 'j':
			if (m + 17 < n)
				m += 17;
			break;
		case KEY_UP:
		case 'k':
			m -= 17;
			if (m < 0)
				m = 0;
			break;
		case 27:
		case ' ':
		case '\n':
			exitnow = 1;
			break;
		default:
			break;
		}
	} while (!exitnow);

	for (i = 0; entries[i] != NULL; i++)
		free(entries[i]);
	free(entries);

	return 0;
}

/*
 * Reads in words out of file "filename" into word. If less than 22
 * words are parsed, all words are freed and 1 is returned.
 * Words cannot be longer than 19 chars. No word duplicates allowed.
 *
 * First line of file "filename" will be ignored (description).
 *
 * Returns ...
 * 0 on success.
 * 1 on xsnprintf failure (file path too long).
 * 2 on failure during file opening (errno set by fopen).
 * 3 if not enough words in wordfile.
 */
#ifndef TEST
static
#endif /* TEST */
int
loadwords(char *filename)
{
	int fd, ignore, k, l;
	char *curpos, *newpos, *p;
	char **pointer;
	char buf[60], wordpath[MAXPATHLEN];
	size_t i, j;
	struct stat sb;

	freewords();

	if (opt.net == NET) {
		if (xsnprintf(buf, sizeof(buf), " WORDLIST %s", filename))
			return 1;
		netsend(buf);
		if (netrecv(1, 1, 1, 1, buf, sizeof(buf)) != 8)
			return 3;

		if ((k = getnum(buf + sizeof(" WORDLIST ") - 1)) < 0)
			return 3;

		words.bulk = xmalloc(j = 1024);

		for (i = 0; k > 0; k--) {
			if (netrecv(1, 1, 1, 0, buf, sizeof(buf)) == -1) {
				freewords();
				return 3;
			}
			buf[sizeof(buf) - 1] = '\0';

			if (i + strlen(buf) + 2 >= j) {
				j += 1024;
				if ((p = realloc(words.bulk, j)) == NULL) {
					freewords();
					return 3;
				}
				words.bulk = p;
			}
			l = snprintf(words.bulk + i, j - i - 1, "%s\n", buf);
			if (l < 0) {
				freewords();
				return 3;
			}
			i += l;
		}
	} else {
		if (xsnprintf(wordpath, sizeof(wordpath), "%s/%s", worddir,
		    filename))
			return 1;

		if ((fd = open(wordpath, O_RDONLY, 0)) == -1)
			return 2;

		if (fstat(fd, &sb) == -1)
			return 2;

		if (sb.st_size >= SSIZE_MAX)
			return 3;

		words.bulk = xmalloc(sb.st_size + 1);

		if (read(fd, words.bulk, sb.st_size) != sb.st_size) {
			freewords();
			return 2;
		}
		words.bulk[sb.st_size] = '\0';

		if (close(fd) == -1)
			xerr(1, "loadwords: fclose: %s", wordpath);
	}

	words.word = xmalloc(sizeof(char *) * 1024);
	words.max = 1024;
	words.n = 0;

	curpos = words.bulk;

	/* ignore title of word list */
	if ((curpos = strchr(curpos, '\n')) == NULL) {
		freewords();
		return 3;
	}

	while ((p = strchr(curpos, '\n')) != NULL) {
		*p = '\0';
		newpos = p + 1;

		if ((p = strchr(curpos, '\r')) != NULL)
			*p = '\0';

		if (curpos[0] == '\0' || strlen(curpos) > 19) {
			curpos = newpos;
			continue;
		}

		for (ignore = 0, p = curpos; *p != '\0'; p++)
			if (!isprint(*p) || isspace(*p)) {
				ignore = 1;
				break;
			}
		if (!ignore)
			words.word[words.n++] = curpos;

		if (words.n >= words.max) {
			if ((size_t)-1 - 1024 < words.max)
				xerrx(1, "loadwords: too many words!");
			pointer = realloc(words.word,
			    (words.max + 1024) * sizeof(char *));
			if (pointer == NULL)
				xerr(1, NULL);
			words.word = pointer;
			words.max += 1024;
		}

		curpos = newpos;
	}

	for (i = 0; i < words.n; i++)
		for (j = i + 1; j < words.n; j++)
			if (!strcmp(words.word[i], words.word[j]))
				words.word[j] = words.word[--words.n];

	if (words.n < 22) {
		/* not enough words */
		freewords();
		return 3;
	}
	return 0;
}

/*
 * Used to fill in option and value out of line. "line" should be a line
 * out of configuration file or rule set file.
 *
 * IMPORTANT: "line" _will_ be changed, as option and value only point
 * at the correct position of line.
 *
 * Returns 0 on success or 1 on failure.
 */
static int
parseline(char *line, char **option, char **value)
{
	char *p, *v;

	/* By default, no option and no value will be returned. */
	*option = *value = NULL;

	/*
	 * First off, the line will be "normalized", i.e. all
	 * unneeded characters (white spaces in front of option,
	 * \n and possible \r and comments [beginning with #]).
	 * If there is no \n, the line was not fully red, so
	 * let's drop it completely.
	 */
	if ((p = strchr(line, '\n')) == NULL)
		return 1;
	*p = '\0';
	if ((p = strchr(line, '\r')) != NULL)
		*p = '\0';
	if ((p = strchr(line, '#')) != NULL)
		*p = '\0';
	while (*line != '\0' && isblank(*line))
		line++;

	/* Anything left? */
	if (line[0] == '\0')
		return 0;

	/*
	 * Locate the first "=" and use p for the oPtion
	 * and v for the Value.
	 */
	if ((v = strchr(line, '=')) == NULL || v == line)
		return 1;

	*v = '\0';
	p = v - 1;
	v++;

	/* Remove all trailing whitespaces between option and equal sign. */
	while (p != line && isblank(*p))
		*(p--) = '\0';

	/* Remove all whitespaces between equal sign and value. */
	while (*v != '\0' && isblank(*v))
		v++;

	/* Remove all trailing whitespaces. */
	if ((p = strchr(v, '\0')) == NULL || p == v)
		return 1;
	p--;
	while (p != v && isblank(*p))
		*(p--) = '\0';

	*option = line;
	*value = v;

	return 0;
}

/*
 * Opens system wide and user specific configuration file. If there is
 * a user configuration file, a user specific high score file will be
 * opened, too. This way, the system wide high score won't be tampered with.
 */
void
readconfig(void)
{
	char *envhome;
	char userhigh[MAXPATHLEN], userconf[MAXPATHLEN];
	struct stat sb;

	if (xsnprintf(ruledir, sizeof(ruledir), "%s", RULEDIR)) {
		ruledir[0] = '.';
		ruledir[1] = '\0';
	}
	if (xsnprintf(worddir, sizeof(worddir), "%s", WORDDIR)) {
		worddir[0] = '.';
		worddir[1] = '\0';
	}

	readfile(CONFIGFILE, 1);

	if ((envhome = getenv("HOME")) == NULL)
		return;

	if (xsnprintf(userconf, sizeof(userconf), "%s/.typespeed/config",
	    envhome))
		return;

	if (stat(userconf, &sb) || (sb.st_mode & S_IFMT) != S_IFREG)
		return;

	if (xsnprintf(userhigh, sizeof(userhigh), "%s/.typespeed/score",
	    envhome))
		return;

	/*
	 * Open a user writable high score.
	 * This can be a symbolic link to system-wide high score
	 * file. Protect system-wide high score file with group
	 * write permissions: privileged gid already dropped.
	 */
	if (close(hfd) == -1)
		xerr(1, "readconfig: close");
	if ((hfd = open(userhigh, O_RDWR, 0)) == -1)
		xerr(1, "readconfig: open: %s", userhigh);

	readfile(userconf, 1);
}

/*
 * Function used to open configuration and game rule files and to
 * set options with function setoptions.
 */
static void
readfile(char *filename, int op)
{
	int i;
	FILE *file;
	unsigned long lineno;
	char *option, *value;
	char buf[1024], line[sizeof("worddir = ") + MAXPATHLEN + 2];

	lineno = 0;

	if (opt.net == NET && !op) {
		if (xsnprintf(buf, sizeof(buf), " RULESET %s", filename))
			return;
		netsend(buf);
		if (netrecv(1, 1, 1, 1, buf, sizeof(buf)) != 6)
			return;

		if ((i = getnum(buf + sizeof(" RULESET ") - 1)) < 0)
			return;

		for (; i >= 0; i--) {
			lineno++;
			if (netrecv(1, 1, 1, 0, line, sizeof(line)) == -1)
				return;
			line[sizeof(line) - 3] = '\0';
			strncat(line, "\n", 1);
			if (parseline(line, &option, &value))
				xerrx(1, "Error in %s line %lu", filename,
				    lineno);
			if (option != NULL && value != NULL)
				setoptions(option, value, op);
		}
	} else {
		if ((file = fopen(filename, "r")) == NULL)
			return;

		while (fgets(line, sizeof(line), file) != NULL) {
			lineno++;
			if (parseline(line, &option, &value))
				xerrx(1, "Error in %s line %lu", filename,
				    lineno);
			if (option != NULL && value != NULL)
				setoptions(option, value, op);
		}

		if (fclose(file) == EOF)
			xerr(1, "readfile: fclose: %s", filename);
	}
}

char *
xstrsep(char **str, char *delim)
{
	char *p, *ret;

	if (*str == NULL)
		return NULL;

	ret = *str;
	for (p = *str; *p != '\0'; p++)
		if (strchr(delim, *p) != NULL)
			break;
	if (*p == '\0') {
		*str = NULL;
		return ret;
	}

	for (*p++ = '\0'; *p != '\0' && strchr(delim, *p) != NULL; p++)
		;
	if (*p == '\0') {
		*str = NULL;
		return ret;
	}

	*str = p;
	return ret;
}

/*
 * setoptions sets actually configuration and rule sets. If a
 * configuration or rule set has been opened decides "op":
 * 0: game rule
 * 1: configuration file
 */
static void
setoptions(char *option, char *value, int op)
{
	int i;
	char *p;
	unsigned long val;

	if (op) { /* configuration file */
		if (!strcmp(option, "cheat")) {
			if (!strcmp(value, "yes"))
				opt.cheat = 1;
			else
				opt.cheat = 0;
		}
		else if (!strcmp(option, "highorder")) {
			for (i = 0; i < 3; i++) {
				if ((p = xstrsep(&value, " \t")) == NULL) {
					printf("break\n");
					break;
				}

				switch (p[0]) {
				case 'c':
				case 's':
				case 't':
					opt.order[i] = p[0];
					break;
				default:
					printf("def\n");
					xerrx(1, "highorder: %s unknown", p);
					/* NOTREACHED */
				}
			}
			if (i != 3 || p == NULL ||
			    (p = xstrsep(&value, " \t")) != NULL) {
				printf("abc\n");
				xerrx(1, "highorder invalid");
			}
		} else if (!strcmp(option, "ruledir"))
			xstrncpy(ruledir, value, sizeof(ruledir) - 1);
		else if (!strcmp(option, "worddir"))
			xstrncpy(worddir, value, sizeof(worddir) - 1);
		else if (!strcmp(option, "rule"))
			xstrncpy(rules.fname, value, sizeof(rules.fname) - 1);
	}
	else { /* game rule */
		if (!strcmp(option, "misses")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "misses invalid: %s", value);
			if (val > 0 && val < 100)
				rules.misses = val;
		}
		else if (!strcmp(option, "min word length")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "min word length invalid: %s",
				    value);
			if (val > 0 && val < 20)
				rules.minlen = val;
		}
		else if (!strcmp(option, "max word length")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "max word length invalid: %s",
				    value);
			if (val > 0 && val < 20)
				rules.maxlen = val;
		}
		else if (!strcmp(option, "min words")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "min words invalid: %s", value);
			if (val > 0 && val < 23)
				rules.minwords = val;
		}
		else if (!strcmp(option, "max words")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "max words invalid: %s", value);
			if (val > 0 && val < 23)
				rules.maxwords = val;
		}
		else if (!strcmp(option, "highscore")) {
			if (!strcmp(value, "yes"))
				rules.hightype = 1;
			else
				rules.hightype = 0;
		}
		else if (!strcmp(option, "min score")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "min score invalid: %s", value);
			rules.minscore = val;
		}
		else if (!strcmp(option, "min speed")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "min speed invalid: %s", value);
			if (val < 100)
				rules.minspeed = val;
		}
		else if (!strcmp(option, "max speed")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "max speed invalid: %s", value);
			if (val < 100)
				rules.maxspeed = val;
		}
		else if (!strcmp(option, "name"))
			xstrncpy(rules.name, value, sizeof(rules.name) - 1);
		else if (!strcmp(option, "step")) {
			if (!(val = cstrl(value)) && errno)
				xerrx(1, "step invalid: %s", value);
			if (val < 1000)
				rules.step = val;
		}
		else if (!strcmp(option, "smooth")) {
			if (!strcmp(value, "yes"))
				rules.smooth = 1;
			else
				rules.smooth = 0;
		}
	}
}

void
setruleset(char *path)
{
	int i, k;
	struct finfo namelist[WORDFILE_MAX];
	char fullpath[MAXPATHLEN];

	k = getfinfo(&(namelist[1]), WORDFILE_MAX - 1, "rule.");
	if (k++ < 1)
		xerrx(1, "setruleset: no rule sets found");
	for (i = 1; i < k; i++)
		if (!strcmp(path, namelist[i].name))
			break;
	if (i == k)
		xerrx(1, "setruleset: supplied default rule set not found");
	if (xsnprintf(fullpath, sizeof(fullpath), "%s/%s", ruledir,
	    namelist[i].name))
		xerrx(1, "setruleset: supplied default rule set name too long");
	readfile(fullpath, 0);
	xstrncpy(rules.fname, namelist[i].name, sizeof(rules.fname) - 1);
	xstrncpy(rules.name, namelist[i].descr, sizeof(rules.name) - 1);
}

/*
 * Removes escaped tabs and returns converted string.
 * This string must be passed to free later on!
 */
#ifndef TEST
static
#endif /* TEST */
char *
unescstr(char *string)
{
	int pos;
	char *new, *p, *retval;

	if (string == NULL)
		return NULL;

	new = xmalloc(strlen(string) + 1);

	for (pos = 0, p = string; *p != '\0'; p++, pos++) {
		if (*p == '\\') {
			if (*(p + 1) == '\0')
				xerrx(1, "error in unescstr");
			else
				p++;
		}
		new[pos] = *p;
	}
	new[pos] = '\0';

	retval = xstrdup(new);
	free(new);

	return retval;
}

