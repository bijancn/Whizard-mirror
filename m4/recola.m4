dnl recola.m4 -- checks for Recola package
dnl

AC_DEFUN([WO_PROG_RECOLA],
[dnl
AC_ARG_ENABLE([recola],
  [AS_HELP_STRING([--enable-recola],
     [(experimental) enable Recola as matrix element generator [[no]]])],
  [], [enable_recola="no"])

AC_ARG_WITH([recola],
  [AS_HELP_STRING([--with-recola=dir],
     [assume the given directory for Recola])])

unset RECOLA_DIR

if test "$enable_recola" = "yes"; then
  
  unset RECOLA_DIR
  if test -n "$with_recola"; then
    WO_PATH_LIB(recola_lib, [recola], [librecola.${SHRLIB_EXT}], ${with_recola})
  else
    WO_PATH_LIB(recola_lib, [recola], [librecola.${SHRLIB_EXT}], $LD_LIBRARY_PATHj)
  fi
  if test "$recola_lib" != "no"; then
    RECOLA_DIR=`dirname $recola_lib`
  fi

else

  AC_MSG_CHECKING([for Recola])
  AC_MSG_RESULT([disabled])

fi

AC_SUBST([RECOLA_DIR])

if test -n "$RECOLA_DIR"; then
  wo_recola_includes="-I$RECOLA_DIR/modules"
  wo_recola_ldflags="-L$RECOLA_DIR -lrecola"
fi

if test "$enable_recola" = "yes" -a "$recola_lib" != "no"; then
    AC_MSG_RESULT([ Recola found])
    RECOLA_AVAILABLE_FLAG=".true."
    RECOLA_INCLUDES=$wo_recola_includes
    LDFLAGS_RECOLA=$wo_recola_ldflags
else
   RECOLA_AVAILABLE_FLAG=".false."
   enable_recola="no"
fi

AC_SUBST([RECOLA_AVAILABLE_FLAG])
AC_SUBST([RECOLA_INCLUDES])
AC_SUBST([LDFLAGS_RECOLA])

AM_CONDITIONAL([RECOLA_AVAILABLE], [test "$enable_recola" = "yes"])

]) dnl WO_PROG_RECOLA
### end WO_PROG_RECOLA 


### Check if the compiler actually supports Recola
AC_DEFUN([WO_FC_CHECK_RECOLA],
[dnl
AC_CACHE_CHECK([whether Recola can be compiled with $FC],
[wo_cv_fc_supports_recola],
[dnl
AC_REQUIRE([WO_PROG_RECOLA])
AC_LANG([Fortran])
FCFLAGS="$RECOLA_INCLUDES"
AC_COMPILE_IFELSE([dnl
  program recolatest
     use recola
  end program recolatest
  ],
  [wo_cv_fc_supports_recola="yes"],
  [wo_cv_fc_supports_recola="no"])
])
FC_SUPPORTS_RECOLA="$wo_cv_fc_supports_recola"
AM_CONDITIONAL([FC_SUPPORTS_RECOLA], [test "$wo_cv_fc_supports_recola" = "yes"])
])
### end WO_FC_CHECK_RECOLA


   
