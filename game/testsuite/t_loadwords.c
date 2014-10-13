/*
 *  typespeed - measures your typing speed
 *  Copyright (C) 1999-2003   Jani Ollikainen  <bestis@iki.fi>
 *                          & Jaakko Manelius  <jman@iki.fi>
 *  Copyright (C) 2006-2007   Tobias Stoeckmann  <tobias@bugol.de>
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

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_SYS_PARAM_H
	#include <sys/param.h>
#endif /* HAVE_SYS_PARAM_H */

#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#ifdef HAVE_DIRENT_H
	#include <dirent.h>
#endif /* HAVE_DIRENT_H */

#include <err.h>
#include <stdio.h>

#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
	#include <string.h>
#endif /* HAVE_STRING_H */

#include "typespeed.h"

extern void	freewords(void);
extern int	loadwords(char *);

extern char	worddir[MAXPATHLEN];

int
main(void)
{
	char conv[2], input[1024], wordpath[MAXPATHLEN];
	int exp_retval, retval;
	size_t exp_count;
	DIR *data;
	FILE *file;
	struct dirent *dp;

	(void)snprintf(worddir, sizeof(worddir), "t_loadwords_data");

	if ((data = opendir("./t_loadwords_data")) == NULL)
		return EXIT_FAILURE;
	while ((dp = readdir(data)) != NULL) {
		if (strlen(dp->d_name) < 3)
			continue;

		/*
		 * Parse expected wordcount out of comment line.
		 */
		(void)snprintf(wordpath, sizeof(wordpath),
		    "t_loadwords_data/%s", dp->d_name);
		if ((file = fopen(wordpath, "r")) == NULL)
			return EXIT_FAILURE;
		memset(input, 0, sizeof(input));
		fgets(input, sizeof(input), file);
		(void)fclose(file);
		exp_count = atoi(input);

		/*
		 * File name begins with expected return value.
		 */
		conv[0] = dp->d_name[0];
		conv[1] = '\0';
		exp_retval = atoi(conv);

		/*
		 * Evaluate test
		 */
		retval = loadwords(dp->d_name);
		if (retval != exp_retval) {
			fprintf(stderr, "%s - error: %d returned\n",
			    dp->d_name, retval);
			freewords();
			continue;
		}
		if (words.n != exp_count)
			fprintf(stderr, "%s - error: n is %zu.\n",
			    dp->d_name, words.n);
		else
			fprintf(stderr, "%s - passed\n", dp->d_name);
		freewords();
	}
	(void)closedir(data);

	return EXIT_SUCCESS;
}

