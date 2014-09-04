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

extern int typorankkaus(float);

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

	for (color = -1, i = -1; i < 100; i++) {
		e = 0;
		a = typorankkaus(i);

		switch(a) {
		case 0:
			if (i > 0)
				e = 1;
			break;
		case 1:
			if (i)
				e = 1;
			break;
		case 2:
			if (i < 1 || i > 2)
				e = 1;
			break;
		case 3:
			if (i < 2 || i > 4)
				e = 1;
			break;
		case 4:
			if (i < 4 || i > 6)
				e = 1;
			break;
		case 5:
			if (i < 6 || i > 8)
				e = 1;
			break;
		case 6:
			if (i < 8 || i > 11)
				e = 1;
			break;
		case 7:
			if (i < 11 || i > 15)
				e = 1;
			break;
		case 8:
			if (i < 15 || i > 20)
				e = 1;
			break;
		case 9:
			if (i < 20 || i > 30)
				e = 1;
			break;
		case 10:
			if (i < 30 || i > 50)
				e = 1;
			break;
		case 11:
			if (i < 50)
				e = 1;
			break;
		default:
			e = 1;
			break;
		}

		switch(color) {
		case 1:
			if (i < 6 || i > 11)
				e = 1;
			break;
		case 3:
			if (i < 20)
				e = 1;
			break;
		case 6:
			if (i > 6)
				e = 1;
			break;
		case 7:
			if (i < 11 || i > 20)
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

