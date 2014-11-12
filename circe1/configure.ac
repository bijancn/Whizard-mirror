dnl $Id: configure.ac 317 2010-04-18 00:31:03Z ohl $
dnl configure.ac -- Configuration script for CIRCE1
dnl =======================================================================
dnl Process this file with autoconf to produce a configure script.
dnl =======================================================================
dnl 
dnl Copyright (C) 1999-2014 by 
dnl     Wolfgang Kilian <kilian@physik.uni-siegen.de>
dnl     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
dnl     Juergen Reuter <juergen.reuter@desy.de>
dnl	with contributions from
dnl 	Christian Speckner <cnspeckn@googlemail.com>
dnl     and others
dnl 
dnl WHIZARD is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by 
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl 
dnl WHIZARD is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
dnl GNU General Public License for more details.
dnl 
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
dnl 
dnl =======================================================================
dnl Environment variables that can be set by the user:
dnl   FC		Fortran compiler
dnl   FCFLAGS		Fortran compiler flags
dnl =======================================================================
dnl 
dnl Start configuration
AC_INIT([circe1],[2.2.3])
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([1.11 color-tests parallel-tests])
AM_MAKE_INCLUDE

########################################################################
### Package-specific initialization

WO_CONFIGURE_SECTION([Start of package configuration])

### Further version information
PACKAGE_DATE="Aug 15 2014"
PACKAGE_STATUS="beta"

AC_SUBST(PACKAGE_DATE)
AC_SUBST(PACKAGE_STATUS)

AC_MSG_NOTICE([Package name: AC_PACKAGE_NAME()])
AC_MSG_NOTICE([Version:      AC_PACKAGE_VERSION()])
AC_MSG_NOTICE([Date:         $PACKAGE_DATE])
AC_MSG_NOTICE([Status:       $PACKAGE_STATUS])

########################################################################
### Required programs and checks

###---------------------------------------------------------------------
### GNU Tools

WO_CONFIGURE_SECTION([Generic tools])

AC_PROG_CC
AC_PROG_RANLIB
AC_PROG_LIBTOOL
AC_PROG_MKDIR_P
LT_INIT
LT_PREREQ([2.2.6b])
AC_PATH_PROG([M4],[m4])

########################################################################
### Enable the distribution tools
### (default: disabled, to speed up compilation)

AC_ARG_ENABLE([distribution],
  [AS_HELP_STRING([--enable-distribution],
    [build the distribution incl. all docu (developers only) [[no]]])])

AC_CACHE_CHECK([whether we want to build the distribution], 
[wo_cv_distribution],
[dnl
if test "$enable_distribution" = "yes"; then
  wo_cv_distribution=yes
else
  wo_cv_distribution=no
fi])

AM_CONDITIONAL([DISTRIBUTION],
  [test "$enable_distribution" = "yes"])

########################################################################
# CIRCE1 options for the configure script
########################################################################

AC_ARG_ENABLE([fortran-dialect],
[  --enable-fortran-dialect    f90 (default) or f95],
[case "${enableval}" in
  *90) FORTRAN_DIALECT=f90 ;;
  *95) FORTRAN_DIALECT=f95 ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-fortran-dialect]) ;;
esac],[FORTRAN_DIALECT=f90])
AC_SUBST([FORTRAN_DIALECT])

###---------------------------------------------------------------------
### Noweb

WO_CONFIGURE_SECTION([Checks for 'noweb' system])

WO_PROG_NOWEB

###---------------------------------------------------------------------
### LaTeX

WO_CONFIGURE_SECTION([Checks for 'LaTeX' system])

### Determine whether LaTeX is present
AC_PROG_LATEX()
AC_PROG_DVIPS()
AC_PROG_PDFLATEX()
AC_PROG_MAKEINDEX()
AC_PROG_PS2PDF()
AC_PROG_EPSPDF()
AC_PROG_EPSTOPDF()
AC_PROG_SUPP_PDF()
AC_PROG_GZIP()
AC_PATH_PROG(ACROREAD,acroread,false)
AC_PATH_PROG(GHOSTVIEW,gv ghostview,false)
AC_PATH_PROG(DOT,dot,false)

### Determine whether Metapost is present 
AC_PROG_MPOST()

########################################################################
### Fortran

WO_CONFIGURE_SECTION([Fortran compiler checks])

user_FCFLAGS="${FCFLAGS}"
AC_PROG_FC()
AC_FC_SRCEXT([f90])
WO_FC_GET_VENDOR_AND_VERSION

### Choose FC standard for MLM matching-interface F77 files
AC_PROG_F77([$FC])
### Work around the `-g' vs. `-g90' confusion at NAG:
### (NB: apparently `echo -n' is not portable (OSX).
###      Let's hope that tr is ...)
if test X"${user_FCFLAGS}" = X; then
  if test X"${FC_VENDOR}" = X"NAG"; then
    FCFLAGS="`for f in ${FCFLAGS}; do case $f in "-g") echo "-g90";; *) echo $f;; esac; done | tr '\n\r' '  '`"
  fi
fi
WO_FC_CHECK_OO_FEATURES()
## WO_FC_CHECK_FLUSH
## WO_FC_CHECK_ISO_FORTRAN_ENV
WO_FC_MODULE_FILE([FC_MODULE_NAME], [FC_MODULE_EXT], [$FC], [f90])
WO_FC_CHECK_EXTENDED
WO_FC_CHECK_EXTENDED_C
WO_FC_SET_PRECISION
WO_FC_SET_OPENMP
WO_FC_SET_PROFILING
AC_CONFIG_FILES([src/kinds.f90])

########################################################################
### Wrapup

WO_CONFIGURE_SECTION([Finalize configuration])

AC_CONFIG_FILES([
  Makefile
  src/Makefile
  minuit/Makefile
  tools/Makefile
  share/Makefile
  share/data/Makefile
  share/doc/Makefile])

AC_OUTPUT
