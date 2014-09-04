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

#include <stdio.h>

#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

extern int level(int);

short color;

void
xcolor_set(short a)
{
	color = a;
}

int
main(void)
{
	int a, e, i;

	for (color = -1, i = -1; i < 900; i++) {
		e = 0;
		a = level(i);

		if (i < 0 && a == 0)
			continue;
		if (i > 900 && a == 10)
			continue;

		switch(a) {
		case 0:
			if (i > 0 || color != 6)
				e = 1;
			break;
		case 1:
			if (i < 0 || i > 100 || color != 6)
				e = 1;
			break;
		case 2:
			if (i < 100 || i > 200 || color != 6)
				e = 1;
			break;
		case 3:
			if (i < 200 || i > 300 || color != 6)
				e = 1;
			break;
		case 4:
			if (i < 300 || i > 400 || color != 6)
				e = 1;
			break;
		case 5:
			if (i < 400 || i > 500 || color != 1)
				e = 1;
			break;
		case 6:
			if (i < 500 || i > 600 || color != 1)
				e = 1;
			break;
		case 7:
			if (i < 600 || i > 700 || color != 7)
				e = 1;
			break;
		case 8:
			if (i < 700 || i > 800 || color != 3)
				e = 1;
			break;
		case 9:
			if (i < 800 || i > 900 || color != 3)
				e = 1;
			break;
		case 10:
			if (i < 900 || color != 3)
				e = 1;
			break;
		default:
			e = 1;
			break;
		}
		if (e)
			fprintf(stderr, "error: %d -> %d\n", i, a);
	}

	return EXIT_SUCCESS;
}

