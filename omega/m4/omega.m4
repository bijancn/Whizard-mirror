dnl omega.m4 -- options for the O'Mega matrix element generator
dnl

### Enable/disable unreleased O'Mega models
AC_DEFUN([WO_SET_OMEGA_UNRELEASED],
[dnl
AC_ARG_ENABLE([omega_unreleased],
  [AC_HELP_STRING([--enable-omega-unreleased],
    [build unreleased Omega modules (not tested extensively yet) [[no]]])])
AM_CONDITIONAL([SELECT_OMEGA_UNRELEASED],
	[test -n "$omega_unreleased"])
])
### end WO_SET_OMEGA_UNRELEASED

### Enable/disable unsupported O'Mega models
AC_DEFUN([WO_SET_OMEGA_UNSUPPORTED],
[dnl
AC_ARG_ENABLE([omega_unsupported],
  [AC_HELP_STRING([--enable-omega-unsupported],
    [build unsupported Omega applications that are still under development [[no]]])])
AM_CONDITIONAL([SELECT_OMEGA_UNRELASED],
	[test "$omega_unsupported" = "yes"])
AM_CONDITIONAL([SELECT_OMEGA_THEORETICAL],
	[test "$omega_unsupported" = "yes"])
AM_CONDITIONAL([SELECT_OMEGA_REDUNDANT],
	[test "$omega_unsupported" = "yes"])
AM_CONDITIONAL([SELECT_OMEGA_DEVELOPERS],
	[test "$omega_unsupported" = "yes"])
AM_CONDITIONAL([SELECT_OMEGA_OBSOLETE],
	[test "$omega_unsupported" = "yes"])
])
### end WO_SET_OMEGA_UNSUPPORTED

AC_DEFUN([WO_SET_OMEGA_THEORETICAL],
[dnl
AC_ARG_ENABLE([omega_theoretical],
  [AC_HELP_STRING([--enable-omega-theoretical],
    [build some theoretical models not realized in nature [[no]]])])
AM_CONDITIONAL([SELECT_OMEGA_THEORETICAL],
	[test "$omega_theoretical" = "yes"])
])
### end WO_SET_OMEGA_THEORETICAL

AC_DEFUN([WO_SET_OMEGA_REDUNDANT],
[dnl
AC_ARG_ENABLE([omega_redundant],
  [AC_HELP_STRING([--enable-omega-redundant],
    [build some redundant models for consistency checks [[no]]])])
AM_CONDITIONAL([SELECT_OMEGA_REDUNDANT],
	[test "$omega_redundant" = "yes"])
])
### end WO_SET_OMEGA_REDUNDANT

AC_DEFUN([WO_SET_OMEGA_DEVELOPERS],
[dnl
AC_ARG_ENABLE([omega_developers],
  [AC_HELP_STRING([--enable-omega-developers],
    [build unreleased components (developers only!) [[no]]])])
AM_CONDITIONAL([SELECT_OMEGA_DEVELOPERS],
	[test "$omega_developers" = "yes" -a $OCAMLINTEGERVERSION -ge 307000])
])
### end WO_SET_OMEGA_DEVELOPERS

AC_DEFUN([WO_SET_OMEGA_OBSOLETE],
[dnl
AC_ARG_ENABLE([omega_obsolete],
  [AC_HELP_STRING([--enable-omega-obsolete],
    [build some obsolete/historical implementations [[no]]])])
AM_CONDITIONAL([SELECT_OMEGA_OBSOLETE],
	[test "$omega_obsolete" = "yes"])
])
### end WO_SET_OMEGA_OBSOLETE

AC_DEFUN([WO_SET_OMEGA_GUI],
[dnl
AC_ARG_ENABLE([omega_gui],
  [AC_HELP_STRING([--enable-omega-gui],
    [build a partial Omega GUI (requires LablGTK!!!)  [[no]]])])
AM_CONDITIONAL([SELECT_OMEGA_GUI],
	[test -n "$LABLGTKDIR" -a "$omega_gui" = "yes"])
])
### end WO_SET_OMEGA_GUI
