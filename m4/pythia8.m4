dnl pythia8.m4 -- checks for PYTHIA 8 library
dnl

AC_DEFUN([WO_PROG_PYTHIA8],
[dnl
AC_REQUIRE([AC_PROG_CXX])

AC_ARG_ENABLE([pythia8],
  [AS_HELP_STRING([--enable-pythia8],
    [enable PYTHIA8 for shower and hadronization [[no]]])],
  [], [enable_pythia8="no"])

if test "$enable_pythia8" = "yes"; then
  ACX_CHECK_PYTHIA8()
  if test "${pyconfig}" = "no"; then
     enable_pythia8="no"
  else
     wo_require_stdcpp="yes"
  fi
else
  AC_MSG_CHECKING([for PYTHIA8])
  AC_MSG_RESULT([(disabled)])
fi

AC_SUBST([PYTHIA8_CXXFLAGS])
AC_SUBST([PYTHIA8_LIBS])

AM_CONDITIONAL([PYTHIA8_AVAILABLE], [test "$enable_pythia8" = "yes"])
])

dnl #####################################################################
dnl CHECK PYTHIA8 BEGIN
dnl
dnl By defaults, this searches the PYTHIA8 library in standard system
dnl locations but an alternative path can be specified using the
dnl --with-pythia8=... configure option
dnl
dnl If PYTHIA8 is found and functional, the variables PYTHIA8_CXXFLAGS
dnl and PYTHIA8_LIBS are set
AC_DEFUN([ACX_CHECK_PYTHIA8],
[
dnl ckeck if a directory is specified for FastJet
AC_ARG_WITH(pythia8,
            [AC_HELP_STRING([--with-pythia8=dir], 
                            [assume the given directory for PYTHIA8])])

dnl search for the pythia8-config script
if test "$with_pythia8" = ""; then
   AC_PATH_PROG(pyconfig, pythia8-config, no)
else
   AC_PATH_PROG(pyconfig, pythia8-config, no, ${with_pythia8}/bin)
fi

if test "${pyconfig}" = "no"; then
   AC_MSG_CHECKING(PYTHIA8)
   AC_MSG_RESULT(no);
   $2
else

   dnl now see if PYTHIA8 is functional
   save_CXXFLAGS="$CXXFLAGS"
   save_LIBS="$LIBS"

   CXXFLAGS="${CXXFLAGS} `${pyconfig} --cxxflags`"
   LIBS="${LIBS} `${pyconfig} --libs`"

   AC_MSG_CHECKING([if PYTHIA8 is functional])
   AC_LANG_PUSH(C++)
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <Pythia8/Pythia.h>
   ]], [[
Pythia8::Pythia* pythia=new Pythia8::Pythia;
   ]])], [pyok='yes'], [pyok='no'])
   AC_MSG_RESULT([$pyok])
   AC_LANG_POP()
   CXXFLAGS="$save_CXXFLAGS"
   LIBS="$save_LIBS"

   AC_MSG_CHECKING(PYTHIA8)
   if test "${pyok}" = "yes"; then
      PYTHIA8_CXXFLAGS="`${pyconfig} --cxxflags`"
      PYTHIA8_LIBS="`${pyconfig} --libs`"
      AC_MSG_RESULT(yes)
      $1
   else
      AC_MSG_RESULT(no)
      $2
   fi
fi
AC_MSG_WARN([PYTHIA8 configure is for testing purposes at the moment.])
])

dnl CHECK PYTHIA8 END