dnl hoppet.m4 -- checks for HOPPET library
dnl

include([aux.m4])

### Determine paths to HOPPET components
### If successful, set the conditional HOPPET_AVAILABLE
### Also: HOPPET_VERSION HOPPET_INCLUDES LDFLAGS_HOPPET
AC_DEFUN([WO_PROG_HOPPET],
[dnl
AC_REQUIRE([AC_PROG_FC])

AC_ARG_ENABLE([hoppet],
  [AS_HELP_STRING([--enable-hoppet],
    [enable HOPPET for b quark pdf matching [[no]]])],
  [], [enable_hoppet="no"])

if test "$enable_hoppet" = "yes"; then

  # Guessing the most likely paths
  wo_hoppet_path="/usr/local/lib:/usr/lib:/opt/local/lib"

  WO_PATH_LIB(HOPPET, hoppet_v1, libhoppet_v1.a, $wo_hoppet_path:$HOPPET_DIR)

  if test "$HOPPET_DIR" = ""; then
    enable_hoppet="no"
  else
    wo_hoppet_includes="-I$HOPPET_DIR/../include/hoppet"
  fi

  if test "$enable_hoppet" = "yes"; then
    wo_hoppet_libdir="-L$HOPPET_DIR"
    AC_LANG([Fortran])
    HOPPET_INCLUDES=$wo_hoppet_includes
    LDFLAGS_HOPPET="$wo_hoppet_libdir -lhoppet_v1"

  else
    AC_MSG_CHECKING([for HOPPET])
    AC_MSG_RESULT([(disabled)])
  fi

else
  AC_MSG_CHECKING([for HOPPET])
  AC_MSG_RESULT([(disabled)])
fi

AC_SUBST([HOPPET_INCLUDES])
AC_SUBST(LDFLAGS_HOPPET)

if test "$enable_hoppet" = "yes"; then
  HOPPET_AVAILABLE_FLAG=".true."
else
  HOPPET_AVAILABLE_FLAG=".false."
fi
AC_SUBST(HOPPET_AVAILABLE_FLAG)

AM_CONDITIONAL([HOPPET_AVAILABLE], [test "$enable_hoppet" = "yes"])
])
