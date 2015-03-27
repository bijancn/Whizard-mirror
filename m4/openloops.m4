dnl openloops.m4 -- checks for gosam package and required helper packages
dnl

AC_DEFUN([WO_PROG_OPENLOOPS],
[dnl
AC_ARG_ENABLE([openloops],
  [AS_HELP_STRING([--enable-openloops],
     [(experimental) enable OpenLoops for NLO matrix elements [[no]]])],
  [], [enable_openloops="no"])

AC_ARG_WITH([openloops],
  [AS_HELP_STRING([--with-openloops=dir],
     [assume the given directory for OpenLoops])])

unset OPENLOOPS_DIR

if test "$enable_openloops" = "yes"; then

  if test "$with_openloops" = ""; then
    AC_PATH_PROG(openloops_lib, [lib/libopenloops.so], [no])
  else
    AC_PATH_PROG(openloops_lib, [lib/libopenloops.so], no, ${with_openloops})
  fi

  if test "$openloops_lib" = "no"; then
    AC_MSG_ERROR([OpenLoops is enabled but not found])
  else
    openloops_libdir=`dirname $openloops_exe`
    OPENLOOPS_DIR=`dirname $openloops_libdir`
    echo "OpenLoops dir is " $OPENLOOPS_DIR
  fi

  save_path=$PATH
  save_ld_library_path=$LD_LIBRARY_PATH

else

  AC_MSG_CHECKING([for OpenLoops])
  AC_MSG_RESULT([(disabled)])

fi

AC_SUBST([OPENLOOPS_DIR])
AM_CONDITIONAL([OPENLOOPS_AVAILABLE], [test "$enable_openloops" = "yes"])

]) dnl WO_PROG_OPENLOOPS




