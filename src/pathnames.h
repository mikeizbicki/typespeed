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
 * pathnames.h - path definitions
 *
 * Please use supplied Makefile to change these values! If you use
 * Makefile, these definitions will be overwritten anyway.
 */

#include <sys/param.h>

#ifndef CONFIGFILE
	#define CONFIGFILE "/etc/typespeedrc"
#endif

#ifndef HIGHFILE
	#define HIGHFILE "/var/games/typespeed.score"
#endif

#ifndef RULEDIR
	#define RULEDIR "/usr/local/share/typespeed/rules"
#endif

#ifndef WORDDIR
	#define WORDDIR	"/usr/local/share/typespeed/words"
#endif

