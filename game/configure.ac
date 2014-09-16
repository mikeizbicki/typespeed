#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(typespeed, 0.6.5, tobias@bugol.de)
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADER([config.h])
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_GNU_SOURCE

# Checks for libraries.
AC_CHECK_LIB([ncurses], [initscr])

# I18N
AM_GNU_GETTEXT_VERSION([0.14.5])
AM_GNU_GETTEXT([external])

# Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h bits/posix1_lim.h fcntl.h libintl.h limits.h locale.h netdb.h netinet/in.h stdarg.h stdlib.h string.h sys/file.h sys/ioctl.h sys/param.h sys/socket.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_UID_T
AC_C_INLINE
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_CLOSEDIR_VOID
AC_PROG_GCC_TRADITIONAL
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STAT
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([gethostbyname gettimeofday inet_ntoa memset setlocale setregid setresgid socket strchr strdup strerror strrchr strstr strtol])

AC_ARG_WITH(highscoredir,
    AC_HELP_STRING([--with-highscoredir=DIR],
        [path to typespeed.score (LOCALSTATEDIR/games)]),
    [ case "$withval" in
        yes|no)
            ;;
        *)
            highscoredir="$withval"
            ;;
        esac],
    highscoredir=${localstatedir}/games)
AC_SUBST(highscoredir)

AC_CONFIG_FILES([Makefile doc/Makefile etc/Makefile man/Makefile po/Makefile.in
                rules/Makefile src/Makefile testsuite/Makefile words/Makefile])
AC_OUTPUT
