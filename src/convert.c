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
 * convert.c - convert your high score for 0.6.0 and later
 */

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#ifdef HAVE_SYS_STAT_H
	#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#ifdef HAVE_DIRENT_H
	#include <dirent.h>
#endif /* HAVE_DIRENT_H */

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

#include "typespeed.h"

struct list {
	int fidelity;
	int pong;
	float cps;
	float tcps;
	float typoratio;
	char faim[21];
};

extern char *escstr(char *);

void usage(const char *);

int
checkspeed(int a, int b, float tcps, float cps)
{
	float speed, tspeed;
	float cspeed, ctspeed;
	clock_t dura;

	for (dura = 1; dura < 100000; dura++) {
		tspeed = (float)a / dura;
		speed = (float)b / dura;

		if (tspeed >= tcps)
			ctspeed = tspeed - tcps;
		else
			ctspeed = tcps - tspeed;

		if (speed >= cps)
			cspeed = speed - cps;
		else
			cspeed = cps - speed;

		if (cspeed < 0.01)
			return dura;
		if (tspeed < tcps - 0.3f)
			return 0;
	}

	return 0;
}

int
convertfile(char *hf, char *nhf)
{
	FILE *highf;
	int i, ver;
	struct list high[10];
	struct stat status;

	memset(&high, 0, sizeof(high));

	printf("Converting %s -> %s ...\n", hf, nhf);
	if (stat(hf, &status)) {
		fprintf(stderr, "Cannot retrieve status of %s\n", hf);
		exit(1);
	}
	if (status.st_size == 410) {
		printf("Found high score file for typespeed < 0.5.0.\n");
		ver = 1;
	} else if (status.st_size == sizeof(struct list) * 10) {
		printf("Found high score file for typespeed 0.5.0 - 0.5.3.\n");
		ver = 2;
	} else {
		fprintf(stderr, "%s: no valid high score file!\n", hf);
		exit(1);
	}
	if ((highf = fopen(hf, "r")) == NULL) {
		fprintf(stderr, "Cannot open %s for reading!\n", hf);
		exit(1);
	}

	if (ver == 1)
		for (i = 0; i < 10; i++) {
			fread(&high[i].pong, sizeof(int), 1, highf);
			fread(&high[i].cps, sizeof(float), 1, highf);
			fread(&high[i].fidelity, sizeof(int), 1, highf);
			fread(&high[i].tcps, sizeof(float), 1, highf);
			fread(&high[i].typoratio, sizeof(float), 1, highf);
			fread(&high[i].faim, sizeof(char), 21, highf);
		}
	else {
		if (fread(&high, sizeof(struct list), 10, highf) != 10) {
			fprintf(stderr, "Error during read of high score.\n");
			exit(1);
		}
	}
	fclose(highf);

	if ((highf = fopen(nhf, "a")) == NULL) {
		fprintf(stderr, "Cannot open %s for writing!\n", nhf);
		exit(1);
	}

	for (i = 0; i < 10; i++) {
		char *fname, *p, *uname;
		int done;
		unsigned long a, b, x, y;
		struct stats nstat;

		/* Convert to new format, but don't convert cheated values. */
		uname = escstr(high[i].faim);
		nstat.score = high[i].pong;
		nstat.wordswritten = 0;
		nstat.duration = 0;

		a = b = nstat.tcount = nstat.score;

		x = 0;
		for (y = 0, done = 0; !done && nstat.score && y < nstat.score; y++) {
			float trat;

			b = nstat.score + y;
			a = (float)b / (1 - (high[i].typoratio) / 100);

			trat = (1.0f - (float)b/a) * 100;
			if (trat == high[i].typoratio) {
				if (nstat.score / y < 20)
					done = checkspeed(a, b, high[i].tcps, high[i].cps);
			} else {
				for (x = 0; x < 1000; x++) {
					trat = (1.0f - (float)b / (a + x)) * 100;
					if (trat == high[i].typoratio) {
						if (nstat.score / y < 20) {
							a += x;
							done = checkspeed(a, b, high[i].tcps, high[i].cps);
						}
					}
					if (trat > high[i].typoratio) {
						if (trat - high[i].typoratio < 0.000002) {
							a += x;
							done = checkspeed(a, b, high[i].tcps, high[i].cps);
						}
						break;
					}
				}
			}
		}

		if (done) {
			y--;

			nstat.wordswritten = y;
			nstat.tcount = a - y;
			nstat.score = b - y;
			nstat.duration = done * 100;
		} else
			continue;

		fname = hf;
		while ((p = strstr(fname, "high.")) != NULL)
			fname = p + strlen("high.");
		if (!strlen(fname)) {
			fprintf(stderr, "Cannot determine word list of %s.\n", hf);
			exit(1);
		}

		fprintf(highf, "%u\t%u\t%u\t%s\t%s\trule.classic\t%u\t0\n",
		    nstat.score, nstat.tcount, nstat.wordswritten, uname, fname,
		    (uint32_t)nstat.duration);

		free(uname);
	}

	fclose(highf);
	return 0;
}

/*
 * All entries in high score file must be valid. If a player
 * has created a word list with a file name that contains a tab
 * (or rule set with tab), the tab must be escaped.
 *
 * Returns converted string that must be passed to free() later on.
 *
 * ADOPTED BY FILE.C
 */
char *
escstr(char *string)
{
	char *new, *p, *retval;
	int pos;

	if (string == NULL)
		return NULL;

	if ((new = calloc(2, strlen(string) + 1)) == NULL) {
		fprintf(stderr, "error in calloc\n");
		exit(1);
	}

	for (pos = 0, p = string; *p != '\0'; p++) {
		if (*p == '\\' || *p == '\t')
			new[pos++] = '\\';
		new[pos++] = *p;
	}
	new[pos] = '\0';

	retval = strdup(new);
	free(new);

	return retval;
}

/*
 * convert will open a high score file if it has the correct size,
 * else the old way of reading stuff can fail. Also we can be
 * rather sure that it must be a high score file ...
 *
 * Returns 0 on success or 1 on failure.
 */
int
main(int argc, char **argv)
{
	DIR *dirp;
	char path[MAXPATHLEN];
	int w;
	struct dirent *dp;

	if (argc != 3)
		usage(argv[0]);

	if ((dirp = opendir(argv[1])) == NULL) {
		fprintf(stderr, "%s: opendir\n", path);
		exit(1);
	}

	while ((dp = readdir(dirp)) != NULL) {
		if (!strncmp(dp->d_name, "high.", strlen("high."))) {
			struct stat sb;

			w = snprintf(path, sizeof(path), "%s/%s", argv[1], dp->d_name);
			if (w < 1 || (size_t)w > sizeof(path) - 1)
				continue;
			if (stat(path, &sb)) {
				fprintf(stderr, "stat: %s", path);
				exit(1);
			}
			if ((sb.st_mode & S_IFMT) != S_IFREG)
				continue;
			convertfile(path, argv[2]);
		}
	}
	(void)closedir(dirp);

	return 0;
}

/* Prints correct usage and exits with return value 1. */
void
usage(const char *program)
{
	fprintf(stderr, "usage: %s old_high_dir new_file\n", program);
	exit(1);
}

