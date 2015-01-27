dnl configure.ac -- Main configuration script for WHIZARD
dnl
dnl Process this file with autoconf to produce a configure script.
dnl ************************************************************************
dnl configure -- Main configuration script for WHIZARD
dnl configure.ac -- WHIZARD configuration
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
dnl Environment variables that can be set by the user:
dnl   FC		Fortran compiler
dnl   FCFLAGS		Fortran compiler flags
dnl 
dnl *********************************************************************** 
dnl Start configuration
AC_INIT([WHIZARD],[2.2.4])
AM_INIT_AUTOMAKE([1.11 color-tests parallel-tests])
AC_PREREQ([2.65])

dnl Caveat: automake internal macro
AM_MAKE_INCLUDE

########################################################################
### Package-specific initialization

AC_MSG_NOTICE([**************************************************************])
WO_CONFIGURE_SECTION([Start of package configuration])

### Further version information
PACKAGE_DATE="Dec 31 2014"
PACKAGE_STATUS="alpha"

AC_SUBST(PACKAGE_DATE)
AC_SUBST(PACKAGE_STATUS)

AC_MSG_NOTICE([**************************************************************])
AC_MSG_NOTICE([Package name: AC_PACKAGE_NAME()])
AC_MSG_NOTICE([Version:      AC_PACKAGE_VERSION()])
AC_MSG_NOTICE([Date:         $PACKAGE_DATE])
AC_MSG_NOTICE([Status:       $PACKAGE_STATUS])
AC_MSG_NOTICE([**************************************************************])

### Dump Package version and date to file 'VERSION'
echo "$PACKAGE_STRING ($PACKAGE_STATUS) $PACKAGE_DATE" \
  > VERSION

########################################################################
### shared library versioning (not the same as the package version!)
LIBRARY_VERSION="-version-info 1:0:0"
AC_SUBST([LIBRARY_VERSION])

########################################################################

### Source directory, for testing purposes
SRCDIR=`cd $srcdir && pwd`
AC_SUBST([SRCDIR])

### Build directory, for testing purposes
BUILDDIR=`pwd`
AC_SUBST([BUILDDIR])

### Location of installed libraries and such
eval BINDIR=$bindir
case $BINDIR in
NONE*) eval BINDIR=$prefix/bin ;;
esac
case $BINDIR in
NONE*) BINDIR="\${prefix}/bin" ;;
esac
AC_SUBST([BINDIR])

eval INCLUDEDIR=$includedir
case $INCLUDEDIR in
NONE*) eval INCLUDEDIR=$prefix/include ;;
esac
case $INCLUDEDIR in
NONE*) INCLUDEDIR="\${prefix}/include" ;;
esac
AC_SUBST([INCLUDEDIR])

### Location of installed libraries and such
eval PKGLIBDIR=$libdir/$PACKAGE
case $PKGLIBDIR in
NONE*) eval PKGLIBDIR=$prefix/lib/$PACKAGE ;;
esac
case $PKGLIBDIR in
NONE*) PKGLIBDIR="\${prefix}/lib/$PACKAGE" ;;
esac
AC_SUBST([PKGLIBDIR])

### Location of installed system-independent data
eval PKGDATADIR=$datarootdir/$PACKAGE
case $PKGDATADIR in
NONE*) eval PKGDATADIR=$prefix/share/$PACKAGE ;;
esac
case $PKGDATADIR in
NONE*) PKGDATADIR="\${prefix}/share/$PACKAGE" ;;
esac
AC_SUBST([PKGDATADIR])

### Location of installed TeX files and such
eval PKGTEXDIR=$datarootdir/texmf/$PACKAGE
case $PKGTEXDIR in
NONE*) eval PKGTEXDIR=$prefix/share/texmf/$PACKAGE ;;
esac
case $PKGTEXDIR in
NONE*) PKGTEXDIR="\${prefix}/share/texmf/$PACKAGE" ;;
esac
AC_SUBST([PKGTEXDIR])

########################################################################
### Required programs and checks

###---------------------------------------------------------------------
### GNU Tools

WO_CONFIGURE_SECTION([Generic tools])

### Initialize LIBTOOL
LT_INIT(dlopen)

LT_PREREQ([2.2.6b])

AX_CHECK_GNU_MAKE()

AC_MSG_CHECKING([for the suffix of shared libraries])
case $host in
  *-darwin* | rhapsody*)
    SHRLIB_EXT="dylib"
    ;;
  cygwin* | mingw* | pw32* | cegcc* | os2*)
    SHRLIB_EXT="dll"
    ;;
  hpux9* | hpux10* | hpux11*)
    SHRLIB_EXT="sl"
    ;;
  *)
    SHRLIB_EXT="so"
    ;;
esac
if test "x$SHRLIB_EXT" != "x"; then
   SHRLIB_EXT=$SHRLIB_EXT
else
   SHRLIB_EXT="so"
fi
AC_MSG_RESULT([.$SHRLIB_EXT])
AC_SUBST(SHRLIB_EXT)

########################################################################
### Host system MAC OS X check for XCode

case $host in 
     *-darwin*)
        WO_HLINE
     	AC_MSG_NOTICE([Host is $host, checking for XCode])	
        AC_PATH_PROG(XCODE_SELECT, xcode-select)
	# locate currently selected Xcode path
	if test "x$XCODE_SELECT" != "x"; then
	  AC_MSG_CHECKING(Xcode location)
	  DEVELOPER_DIR=`$XCODE_SELECT -print-path`
	  AC_MSG_RESULT([$DEVELOPER_DIR])
	else
	  DEVELOPER_DIR=/Developer
	fi
	AC_SUBST(DEVELOPER_DIR)
	
	XCODEPLIST=$DEVELOPER_DIR/Applications/Xcode.app/Contents/version.plist
	if test -r "$XCODEPLIST"; then
	  AC_MSG_CHECKING(Xcode version)
	  if test "x$DEFAULTS" != "x"; then
	    XCODE_VERSION=`$DEFAULTS read $DEVELOPER_DIR/Applications/Xcode.app/Contents/version CFBundleShortVersionString`
	  else
	    XCODE_VERSION=`tr -d '\r\n' < $XCODEPLIST | sed -e 's/.*<key>CFBundleShortVersionString<\/key>.<string>\([[0-9.]]*\)<\/string>.*/\1/'`
	  fi
	  AC_MSG_RESULT([$XCODE_VERSION])
	  AC_SUBST(XCODE_VERSION)
	fi
	WO_HLINE
	;;	

     *)
        ;;
     esac

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

if test "$enable_shared" = no; then
  AC_MSG_ERROR([you've used --disable-shared which will not produce a working Whizard.])
fi

########################################################################
### We include the m4 macro tool here
AC_PATH_PROG(M4,m4,false)
if test "$M4" = false; then
   AM_CONDITIONAL([M4_AVAILABLE],[false])
else
   AM_CONDITIONAL([M4_AVAILABLE],[true])
fi

###---------------------------------------------------------------------
### Dynamic runtime linking

WO_CONFIGURE_SECTION([Dynamic runtime linking])

### Look for libdl (should provide 'dlopen' and friends)
AC_PROG_CC()
WO_PROG_DL()

### Define the conditional for static builds

if test "$enable_static" = yes; then
   AM_CONDITIONAL([STATIC_AVAILABLE],[true])
else
   AM_CONDITIONAL([STATIC_AVAILABLE],[false])
fi

###---------------------------------------------------------------------
### Noweb

WO_CONFIGURE_SECTION([Checks for 'noweb' system])

### Enable/disable noweb and determine locations of notangle, cpif, noweave
WO_PROG_NOWEB()

###---------------------------------------------------------------------
### LaTeX

WO_CONFIGURE_SECTION([Checks for 'LaTeX' system])

### Determine whether LaTeX is present
AC_PROG_LATEX()
AC_PROG_DVIPS()
AC_PROG_PDFLATEX()
AC_PROG_PS2PDF()
AC_PROG_EPSPDF()
AC_PROG_EPSTOPDF()
AC_PROG_SUPP_PDF()
AC_PROG_GZIP()
AC_PATH_PROG(ACROREAD,acroread,false)
AC_PATH_PROG(GHOSTVIEW,gv ghostview,false)
AC_PATH_PROG(DOT,dot,false)

### Determine whether Metapost is present and whether event display is possible
AC_PROG_MPOST()
WO_CHECK_EVENT_ANALYSIS_METHODS()

### We put here the check for HEVEA components as well
WO_PROG_HEVEA()

###---------------------------------------------------------------------
### Fortran compiler

WO_CONFIGURE_SECTION([Fortran compiler checks])

### Determine default compiler to use
user_FCFLAGS="${FCFLAGS}"
AC_PROG_FC()

### Choose FC standard for PYTHIA6 F77 files
AC_PROG_F77([$FC])

### Determine compiler vendor and version
WO_FC_GET_VENDOR_AND_VERSION()

### Veto against gfortran 4.5/4.6/4.7.[0123]
WO_FC_VETO_GFORTRAN_456()
WO_FC_VETO_GFORTRAN_470123()

### Work around the `-g' vs. `-g90' confusion at NAG:
### (NB: apparently `echo -n' is not portable (OSX).
###      Let's hope that tr is ...)
if test X"${user_FCFLAGS}" = X; then
  if test X"${FC_VENDOR}" = X"NAG"; then
    FCFLAGS="`for f in ${FCFLAGS}; do case $f in "-g") echo "-g90";; *) echo $f;; esac; done | tr '\n\r' '  '`"
  fi
fi

### Require extension '.f90' for all compiler checks
AC_FC_SRCEXT([f90])

### Determine flags and extensions
WO_FC_PARAMETERS()

### Determine flags for linking the Fortran runtime library
WO_FC_LIBRARY_LDFLAGS()

### Check for Fortran 95 features
WO_FC_CHECK_F95()

### Check for allocatable subobjects (TR15581)
WO_FC_CHECK_TR15581()

### Check for allocatable scalars
WO_FC_CHECK_ALLOCATABLE_SCALARS()

### Check for ISO C binding support
WO_FC_CHECK_C_BINDING()

### Check for procedures pointers and abstract interfaces
WO_FC_CHECK_PROCEDURE_POINTERS()

### Check for type extension and further OO features
WO_FC_CHECK_OO_FEATURES()

### Check for F2003 command-line interface
WO_FC_CHECK_CMDLINE()

### Check for F2003-style access to environment variables
WO_FC_CHECK_ENVVAR()

### Check for the flush statement
WO_FC_CHECK_FLUSH()

### Check for iso_fortran_env
WO_FC_CHECK_ISO_FORTRAN_ENV()

### Check for extended precision support
WO_FC_CHECK_EXTENDED()

### Check for extended precision support in the C interface
WO_FC_CHECK_EXTENDED_C()

### Extended precision enforced upon request
WO_FC_SET_PRECISION()

### Warn about gfortran 4.7 (currently not necessary)
# WO_FC_WARN_GFORTRAN_47()

### OpenMP threading activated upon request
WO_FC_SET_OPENMP()

### Profiling compilation enforced upon request
WO_FC_SET_PROFILING()

### Impure subroutines enforced upon request
WO_FC_SET_OMEGA_IMPURE()

### Find the extension of Fortran module files
WO_FC_MODULE_FILE([FC_MODULE_NAME], [FC_MODULE_EXT], [$FC], [f90])


###---------------------------------------------------------------------
### O'Caml

WO_CONFIGURE_SECTION([Objective Caml checks])

### Check for ocamlc and its relatives
AC_PROG_OCAML()
if test "$enable_ocaml" != "no"; then
   AC_OCAML_VERSION_CHECK(312000)
   AC_PROG_OCAMLLEX()
   AC_PROG_OCAMLYACC()
   AC_PROG_OCAMLCP()
   ### Ocamlweb is required to be newer than v0.9
   AC_PROG_OCAMLWEB(009000)
   AC_PROG_OCAML_LABLGTK()
   AC_PATH_PROGS(OCAMLDOT,ocamldot)
   AC_PATH_PROGS(OCAMLDEFUN,ocamldefun)
else
   AC_MSG_NOTICE([WARNING: O'Caml and O'Mega matrix elements disabled by request!])
   AM_CONDITIONAL([OCAMLWEB_AVAILABLE],[false])
fi


###---------------------------------------------------------------------
### C++

WO_CONFIGURE_SECTION([C++ compiler checks])
AC_PROG_CXX()

########################################################################
### Libraries

###---------------------------------------------------------------------
### LHAPDF

WO_CONFIGURE_SECTION([LHAPDF])

WO_PROG_LHAPDF()

###---------------------------------------------------------------------
### HepMC

WO_CONFIGURE_SECTION([HepMC])

WO_PROG_HEPMC()

###---------------------------------------------------------------------
### STDHEP

WO_CONFIGURE_SECTION([STDHEP])

WO_PROG_STDHEP()

###---------------------------------------------------------------------
### LCIO

WO_CONFIGURE_SECTION([LCIO])

WO_PROG_LCIO()

###---------------------------------------------------------------------
### PYTHIA6 etc

WO_CONFIGURE_SECTION([SHOWERS PYTHIA6 MPI])

WO_PROG_QCD()

###---------------------------------------------------------------------
### HOPPET

WO_CONFIGURE_SECTION([HOPPET])

WO_PROG_HOPPET()

###---------------------------------------------------------------------
### FASTJET

WO_CONFIGURE_SECTION([FASTJET])

WO_PROG_FASTJET()

###---------------------------------------------------------------------
### GoSam

WO_CONFIGURE_SECTION([GOSAM])

WO_PROG_GOSAM()

###---------------------------------------------------------------------
### Extra flags for helping the linker finding libraries

WO_CONFIGURE_SECTION([Handle linking with C++ libraries])

WO_PROG_STDCPP()

########################################################################
### Miscellaneous

WO_CONFIGURE_SECTION([Numerical checks])

###---------------------------------------------------------------------
### Disable irrelevant optimization for parameter files
### (default: disabled, to speed up compilation)

AC_ARG_ENABLE([optimization-for-parameter-files],
  [AS_HELP_STRING([--enable-optimization-for-parameter-files],
    [enable (useless) optimization for parameter files [[no]]])])

AC_CACHE_CHECK([whether we want optimization for parameter files], 
[wo_cv_optimization_for_parfiles],
[dnl
if test "$enable_optimization_for_parameter_files" = "yes"; then
  wo_cv_optimization_for_parfiles=yes
else
  wo_cv_optimization_for_parfiles=no
fi])

AM_CONDITIONAL([OPTIMIZATION_FOR_PARFILES],
  [test "$enable_optimization_for_parameter_files" = "yes"])


###---------------------------------------------------------------------
### Disable default checks upon request

AC_ARG_ENABLE([default_checks],
  [AS_HELP_STRING([--disable-default-checks],
    [disable the default self-checks [[no]]])])

AC_CACHE_CHECK([whether we want the default self-checks], 
[wo_cv_default_checks],
[dnl
if test "$enable_default_checks" != "no"; then
  wo_cv_default_checks=yes
else
  wo_cv_default_checks=no
fi])

AM_CONDITIONAL([DEFAULT_CHECKS],
  [test "$enable_default_checks" != "no"])

###---------------------------------------------------------------------
### Enable numerical checks upon request

AC_ARG_ENABLE([extnum_checks],
  [AS_HELP_STRING([--enable-extnum-checks],
    [enable time-consuming extensive SM, SUSY and models checks [[no]]])])

AC_CACHE_CHECK([whether we want extra extensive checks], 
[wo_cv_extnum_checks],
[dnl
if test "$enable_extnum_checks" = "yes"; then
  wo_cv_extnum_checks=yes
else
  wo_cv_extnum_checks=no
fi])

AM_CONDITIONAL([EXT_NUM_CHECKS],
  [test "$enable_extnum_checks" = "yes"])

###---------------------------------------------------------------------
### Checks for external interfaces

WO_CONFIGURE_SECTION([Auxiliary stuff for external interfaces])

AX_PYTHON()

########################################################################
### Wrapup

WO_CONFIGURE_SECTION([Finalize configuration])

###--------------------------------------------------------------------
### Main directory

AC_CONFIG_FILES([Makefile])

###---------------------------------------------------------------------
### Subdirectory src

AC_CONFIG_FILES([src/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/hepmc

AC_CONFIG_FILES([src/hepmc/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/lcio

AC_CONFIG_FILES([src/lcio/Makefile])

###---------------------------------------------------------------------
### Subdirectory pythia6: WHIZARD's internal PYTHIA6 version

AC_CONFIG_FILES([pythia6/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/muli: multiple interactions

AC_CONFIG_FILES([src/muli/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/lhapdf5: dummy library as LHAPDF5 replacement

AC_CONFIG_FILES([src/lhapdf5/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/lhapdf: LHAPDF v6

AC_CONFIG_FILES([src/lhapdf/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/pdf_builtin: Builtin PDFs

AC_CONFIG_FILES([src/pdf_builtin/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/stdhep: dummy library as STDHEP replacement

AC_CONFIG_FILES([src/stdhep/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/hoppet

AC_CONFIG_FILES([src/hoppet/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/fastjet

AC_CONFIG_FILES([src/fastjet/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/shower: shower and all that

AC_CONFIG_FILES([src/shower/Makefile])


###---------------------------------------------------------------------
### Subpackage omega

AC_CONFIG_SUBDIRS([omega])

###---------------------------------------------------------------------
### Subpackage vamp

AC_CONFIG_SUBDIRS([vamp])

###---------------------------------------------------------------------
### Subpackage circe1

AC_CONFIG_SUBDIRS([circe1])

###---------------------------------------------------------------------
### Subdirectory circe2

AC_CONFIG_SUBDIRS([circe2])

###---------------------------------------------------------------------
### Subdirectory src/noweb-frame: frame for whizard Noweb sources

AC_CONFIG_FILES([src/noweb-frame/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/basics: numeric kinds, strings

AC_CONFIG_FILES([src/basics/Makefile] [src/basics/kinds.f90])

###---------------------------------------------------------------------
### Subdirectory src/utilities: simple utilities

AC_CONFIG_FILES([src/utilities/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/testing: unit-test support

AC_CONFIG_FILES([src/testing/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/system: modules related to local setup and OS issues

AC_CONFIG_FILES([src/system/Makefile])

AC_CONFIG_FILES([src/system/system_dependencies.f90], [
   maxlen=70
   i=1
   pat=""
   while test ${i} -lt ${maxlen}; do pat="${pat}."; i=`expr ${i} + 1`; done
   pat=${pat}[[^\"]]
   pat="/^       \"${pat}/ s/${pat}/&\&\\
       \&/g"
   $SED "${pat}" < src/system/system_dependencies.f90 > \
      src/system/system_dependencies.tmp
   mv -f src/system/system_dependencies.tmp src/system/system_dependencies.f90
])

###---------------------------------------------------------------------
### Subdirectory src/combinatorics: standard algorithms

AC_CONFIG_FILES([src/combinatorics/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/parsing: text-handling and parsing

AC_CONFIG_FILES([src/parsing/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/rng: random-number generation

AC_CONFIG_FILES([src/rng/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/expr_base: abstract expressions

AC_CONFIG_FILES([src/expr_base/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/physics: particle-physics related functions

AC_CONFIG_FILES([src/physics/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/qft: quantum (field) theory concepts as data types

AC_CONFIG_FILES([src/qft/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/types: HEP and other types for common use

AC_CONFIG_FILES([src/types/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/matrix_elements: process code and libraries

AC_CONFIG_FILES([src/matrix_elements/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/me_methods: specific process code and interface

AC_CONFIG_FILES([src/me_methods/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/particles: particle objects

AC_CONFIG_FILES([src/particles/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/beams: beams and beam structure

AC_CONFIG_FILES([src/beams/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/events: generic events and event I/O

AC_CONFIG_FILES([src/events/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/mci: multi-channel integration and event generation

AC_CONFIG_FILES([src/mci/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/phase_space: parameterization and evaluation

AC_CONFIG_FILES([src/phase_space/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/user: user plugin support

AC_CONFIG_FILES([src/user/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/blha: BLHA support (NLO data record)

AC_CONFIG_FILES([src/blha/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/gosam: GoSAM support (NLO amplitudes)

AC_CONFIG_FILES([src/gosam/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/fks: FKS subtraction algorithm

AC_CONFIG_FILES([src/fks/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/variables: Implementation of variable lists

AC_CONFIG_FILES([src/variables/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/model_features: Model access and methods

AC_CONFIG_FILES([src/model_features/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/models: Model-specific code

AC_CONFIG_FILES([src/models/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/models/SM_tt_threshold_bundle

AC_CONFIG_FILES([src/models/SM_tt_threshold_bundle/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/models/threeshl_bundle

AC_CONFIG_FILES([src/models/threeshl_bundle/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/whizard-core

AC_CONFIG_FILES([src/whizard-core/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/prebuilt

AC_CONFIG_FILES([src/prebuilt/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/feynmf

AC_CONFIG_FILES([src/feynmf/Makefile])

###---------------------------------------------------------------------
### Subdirectory src/gamelan: WHIZARD graphics package

AC_CONFIG_FILES([src/gamelan/Makefile])

AC_CONFIG_FILES([src/gamelan/gml], [chmod u+x src/gamelan/gml])
###---------------------------------------------------------------------
### Subdirectory share

AC_CONFIG_FILES([share/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/doc

AC_CONFIG_FILES([share/doc/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/models

AC_CONFIG_FILES([share/models/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/cuts

AC_CONFIG_FILES([share/cuts/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/beam-sim

AC_CONFIG_FILES([share/beam-sim/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/susy

AC_CONFIG_FILES([share/susy/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/examples

AC_CONFIG_FILES([share/examples/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/tests

AC_CONFIG_FILES([share/tests/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/muli

AC_CONFIG_FILES([share/muli/Makefile])

###---------------------------------------------------------------------
### Subdirectory share/interfaces

AC_CONFIG_FILES([share/interfaces/Makefile])
AC_CONFIG_FILES([share/interfaces/py_whiz_setup.py])

###---------------------------------------------------------------------
### Subdirectory tests

AC_CONFIG_FILES([tests/Makefile])

AC_CONFIG_FILES([tests/run_whizard.sh], [chmod u+x tests/run_whizard.sh])

###---------------------------------------------------------------------
### Subdirectory libtool-config

AC_CONFIG_SUBDIRS([libtool-config])

###--------------------------------------------------------------------
### Subdirectory scripts

AC_CONFIG_FILES([scripts/Makefile])

AC_CONFIG_FILES([scripts/whizard-config], [chmod u+x scripts/whizard-config])
AC_CONFIG_FILES([scripts/whizard-setup.sh], [chmod u+x scripts/whizard-setup.sh])
AC_CONFIG_FILES([scripts/whizard-setup.csh], [chmod u+x scripts/whizard-setup.csh])
AC_CONFIG_FILES([scripts/libtool-config.sh], [chmod u+x scripts/libtool-config.sh])
AC_CONFIG_FILES([scripts/libtool-relocate.sh], [chmod u+x scripts/libtool-relocate.sh])
AC_CONFIG_FILES([scripts/whizard-relocate.sh], [chmod u+x scripts/whizard-relocate.sh])
 
###--------------------------------------------------------------------

AC_OUTPUT()

########################################################################

