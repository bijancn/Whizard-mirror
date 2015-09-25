dnl configure.ac -- Main configuration script for O'Mega
dnl $Id:$
dnl
dnl Process this file with autoconf to produce a configure script.
dnl
dnl ************************************************************************
dnl 
dnl Copyright (C) 1999-2015 by 
dnl     Wolfgang Kilian <kilian@physik.uni-siegen.de>
dnl     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
dnl     Juergen Reuter <juergen.reuter@desy.de>
dnl     with contributions from
dnl     Fabian Bach <fabian.bach@desy.de>
dnl     Bijan Chokoufe <bijan.chokoufe@desy.de>
dnl     Christian Speckner <cnspeckn@googlemail.com>
dnl     Marco Sekulla <marco.sekulla@desy.de>
dnl     Christian Weiss <christian.weiss@desy.de>
dnl     Felix Braam, Sebastian Schmidt,
dnl     Hans-Werner Boschmann, Daniel Wiesler
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
dnl *********************************************************************** 
dnl 
dnl Environment variables that can be set by the user:
dnl   FC		Fortran compiler
dnl   FCFLAGS		Fortran compiler flags
dnl   OCAMLFLAGS	OCaml compiler flags
dnl 
dnl *********************************************************************** 

AC_INIT([omega],[2.2.8])
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([1.11 color-tests parallel-tests])

dnl Caveat: automake internal macro
AM_MAKE_INCLUDE

########################################################################
### Package-specific initialization

WO_CONFIGURE_SECTION([Start of package configuration])

### Further version information
PACKAGE_DATE="Sep 30 2015"
PACKAGE_STATUS="alpha"

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
LT_PREREQ([2.2.8b])
AC_PATH_PROG([M4],[m4])

dnl ***********************************************************************

AM_CONDITIONAL([STANDALONE_OMEGA_BUILD], [test "yes" = "yes"])

########################################################################
### Location of installed libraries and such
### capitalized version is internal, the other
### one for autotools use

eval LIBDIR=$libdir
case $LIBDIR in
NONE*) eval LIBDIR=$prefix/lib ;;
esac
case $LIBDIR in
NONE*) eval LIBDIR=$ac_default_prefix/lib ;;
esac
AC_SUBST([LIBDIR])

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
# O'Mega options for the configure script
########################################################################

AC_ARG_ENABLE([install-all-caches],
[  --enable-install-all-caches
                          Create the complete set of cache files for O'Mega
                          (including more exotic models).  This takes some
                          time (make -j helps), but speeds up the self tests
                          (i.e. make check) later [[default=no]].],
[case "$enableval" in
   yes|no) OMEGA_INSTALL_ALL_CACHES="$enableval"
           ;;
        *) OMEGA_INSTALL_ALL_CACHES=no
           AC_MSG_WARN([Invalid argument to --enable-install-all-caches, using default=no])
           ;;
esac],
[eval OMEGA_INSTALL_ALL_CACHES=no])
AM_CONDITIONAL([OMEGA_INSTALL_ALL_CACHES], [test "$OMEGA_INSTALL_ALL_CACHES" = "yes"])

AC_ARG_ENABLE([system-cache],
[  --enable-system-cache=directory
                          Read precomputed model tables from this directory,
                          which will be populated by an administrator at
                          install time [[default=$localstatedir/cache, enabled]].],
[case "$enableval" in
   no) OMEGA_SYSTEM_CACHE_DIR="."
       ;;
    *) OMEGA_SYSTEM_CACHE_DIR="$enableval"
       ;;
esac],
[### use eval b/c $localstatedir defaults to unexpanded ${prefix}/var
case "$OMEGA_SYSTEM_CACHE_DIR" in
   "") OMEGA_SYSTEM_CACHE_DIR="${prefix}/var/cache"
      ;;
   *) eval OMEGA_SYSTEM_CACHE_DIR="$localstatedir/cache"
      ;;
esac])
AC_SUBST([OMEGA_SYSTEM_CACHE_DIR])

AC_ARG_ENABLE([user-cache],
[  --enable-user-cache=directory
                          Store precomputed model tables in this directory
                          and read them to avoid lengthy recomputations.
                          Leading "~", "$HOME" and "${HOME}" will be replaced
                          at runtime by the expansion of ${HOME}.
                          [[Must be user writable, default=~/.whizard/var/cache,
                           enabled]].],
[case "$enableval" in
   no) OMEGA_USER_CACHE_DIR="."
       ;;
    *) OMEGA_USER_CACHE_DIR="$enableval"
       ;;
esac],
[OMEGA_USER_CACHE_DIR="${HOME}/.whizard/var/cache"])
AC_SUBST([OMEGA_USER_CACHE_DIR])

case "$OMEGA_SYSTEM_CACHE_DIR" in
  .|""|NONE*) OMEGA_SYSTEM_CACHE_DIR="."
              ;;
           *) AC_MSG_NOTICE([Creating system cache directory $OMEGA_SYSTEM_CACHE_DIR])
              $MKDIR_P "$OMEGA_SYSTEM_CACHE_DIR" 2>/dev/null
              chmod u+w "$OMEGA_SYSTEM_CACHE_DIR" 2>/dev/null
              ;;
esac

case "$OMEGA_USER_CACHE_DIR" in
  .|""|NONE*) OMEGA_USER_CACHE_DIR="."
              ;;
           *) AC_MSG_NOTICE([Creating user cache directory $OMEGA_USER_CACHE_DIR])
              $MKDIR_P "$OMEGA_USER_CACHE_DIR" 2>/dev/null
              chmod u+w "$OMEGA_USER_CACHE_DIR" 2>/dev/null
              ;;
esac

OMEGA_CACHE_SUFFIX="vertices"
AC_SUBST([OMEGA_CACHE_SUFFIX])

###---------------------------------------------------------------------
### Noweb

WO_CONFIGURE_SECTION([Checks for 'noweb' system])

### Enable/disable noweb and determine locations of notangle, cpif, noweave
WO_PROG_NOWEB()

###---------------------------------------------------------------------
### LaTeX

WO_CONFIGURE_SECTION([Checks for 'LaTeX' system])

AC_PROG_LATEX()
AC_PROG_DVIPS()
AC_PROG_PDFLATEX()
AC_PROG_PS2PDF()
AC_PROG_EPSPDF()
AC_PROG_EPSTOPDF()
if test "$EPSPDF" = "no" -a "$EPSTOPDF" = "no"; then
   ### test "$EPSPDF" != "no" -a $EPSPDFINTEGERVERSION -lt 040003 -a "$EPSTOPDF" = "no" || \
###   || test "$EPSPDF" = "no" -a "$EPSTOPDF" != "no" -a "$EPSTOPDF_BUGGY" = "yes" || 
###   test "$EPSPDF" != "no" -a $EPSPDFINTEGERVERSION -lt 040003 -a \
###       "$EPSTOPDF" != "no" -a "$EPSTOPDF_BUGGY" = "yes" \
   AC_MSG_NOTICE([*********************************************************])
   AC_MSG_NOTICE([WARNING: eps(to)pdf n/a; O'Mega documentation will crash!])
   AC_MSG_NOTICE([WARNING:  this applies only to the svn developer version!])
   AC_MSG_NOTICE([*********************************************************])
fi
AC_PROG_SUPP_PDF()
AC_PROG_GZIP()
AC_PATH_PROG(ACROREAD,acroread,false)
AC_PATH_PROG(GHOSTVIEW,gv ghostview,false)
AC_PATH_PROG(DOT,dot,false)

### Determine whether Metapost is present
AC_PROG_MPOST()

###---------------------------------------------------------------------
### Fortran compiler

WO_CONFIGURE_SECTION([Fortran compiler checks])

AC_LANG([Fortran])
user_FCFLAGS="${FCFLAGS}"
AC_PROG_FC
AC_FC_SRCEXT([f90])
WO_FC_GET_VENDOR_AND_VERSION
AC_OPENMP
WO_FC_PARAMETERS()
WO_FC_LIBRARY_LDFLAGS()
WO_FC_CHECK_F95()
WO_FC_CHECK_TR19767()
WO_FC_MODULE_FILE([FC_MODULE_NAME], [FC_MODULE_EXT], [$FC], [f90])
WO_FC_CONFIGURE_KINDS([src/kinds.f90])
WO_FC_SET_OPENMP()
WO_FC_SET_PROFILING()
WO_FC_SET_OMEGA_IMPURE()


###---------------------------------------------------------------------
### O'Caml

WO_CONFIGURE_SECTION([Objective Caml checks])

AC_PROG_OCAML
if test "$enable_ocaml" != "no"; then
   ### O'Caml is required to be newer than v3.10
   AC_OCAML_VERSION_CHECK(312000)
   AC_PROG_OCAMLLEX
   AC_PROG_OCAMLYACC
   AC_PROG_OCAMLCP
   ### Ocamlweb is required to be newer than v0.9
   AC_PROG_OCAMLWEB(009000)
   AC_PROG_OCAML_LABLGTK
   AC_PATH_PROGS([OCAMLDOT],[ocamldot],[no])
   AM_CONDITIONAL([OCAMLDOT_AVAILABLE],[test "$OCAMLDOT" != "no"])
   AC_PATH_PROGS([OCAMLDEP],[ocamldep],[no])
   AM_CONDITIONAL([OCAMLDEP_AVAILABLE],[test "$OCAMLDEP" != "no"])
else
   AC_MSG_NOTICE([WARNING: O'Caml and O'Mega matrix elements disabled by request!])
   AM_CONDITIONAL([OCAMLWEB_AVAILABLE],[false])
   AM_CONDITIONAL([OCAMLDOT_AVAILABLE],[false])
   AM_CONDITIONAL([OCAMLDEP_AVAILABLE],[false])
fi

###---------------------------------------------------------------------
### Model files

AC_ARG_ENABLE([model-files],
  [AS_HELP_STRING([--enable-model-files],
    [build the development code for model files (developers only) [[no]]])])

AC_CACHE_CHECK([whether we want to build the development code for model files], 
[wo_cv_model_files],
[dnl
if test "$enable_model_files" = "yes"; then
  wo_cv_model_files=yes
else
  wo_cv_model_files=no
fi])

AM_CONDITIONAL([MODEL_FILES],
  [test "$enable_model_files" = "yes"])

if test "$enable_model_files" = "yes"; then
  comment_model_file=""
else
  comment_model_file="#"
fi
AC_SUBST([comment_model_file])

########################################################################
# Copy config.mli to the build directory (otherwise ocamlc and/or
# ocamlopt would create one on their own).
########################################################################

AC_CONFIG_FILES([src/config.ml])

case "$srcdir" in
 .) ;;
 *) $MKDIR_P ./src
    rm -f ./src/config.mli
    cp $srcdir/src/config.mli ./src/config.mli 1>/dev/null 2>&1;;
esac

dnl ***********************************************************************

AC_CONFIG_FILES([
  Makefile
  bin/Makefile
  lib/Makefile
  models/Makefile
  src/Makefile    
  share/Makefile
  share/doc/Makefile
  extensions/Makefile
  extensions/people/Makefile
  extensions/people/jr/Makefile
  extensions/people/tho/Makefile
  tests/Makefile
  tests/MSSM/Makefile
  tests/SM/Makefile
  tests/people/Makefile
  tests/people/jr/Makefile
  tests/people/tho/Makefile
  tools/Makefile
  scripts/Makefile])
AC_CONFIG_FILES([scripts/omega-config], [chmod u+x scripts/omega-config])

dnl ***********************************************************************
AC_OUTPUT
dnl ***********************************************************************


