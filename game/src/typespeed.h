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
 * typespeed.h - main header file
 */

#include <sys/param.h>

#include <stdint.h>
#include <stdio.h>

#ifdef WIN32
	#include <dirent.h>
	#include <time.h>

	#ifndef MAXHOSTNAMELEN
		#define MAXHOSTNAMELEN	256
	#endif /* MAXHOSTNAMELEN */
#endif /* WIN32 */

#define H2H	1
#define NET	2

struct finfo {
	char descr[61];
	char name[MAXPATHLEN];
};

struct stats {
	uint8_t level;
	uint32_t score;
	uint32_t tcount;
	uint32_t wordswritten;
	uint32_t wpm;
	float ratio;
	float speed;
	clock_t duration;
	unsigned int sinit;
	char name[21];
} now;

struct opt {
	int cheat;
	int net;
	int port;
	unsigned int seed;
	int usecolors;
	char name[21];
	char order[3];
} opt;

struct rules {
	int misses;
	int minlen;
	int maxlen;
	int minwords;
	int maxwords;
	int hightype;
	unsigned long minscore;
	int minspeed;
	int maxspeed;
	int step;
	int smooth;
	float spd_multi;
	int mintime;
	int maxtime;
	char name[31];
	char fname[FILENAME_MAX + 1];
} rules;

struct rawdata {
	char *bulk;
	char **word;
	size_t n;
	size_t max;
} words;

