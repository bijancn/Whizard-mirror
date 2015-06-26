## Makefile.am -- Makefile for executable WHIZARD test scripts
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2015 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
#     Fabian Bach <fabian.bach@desy.de>
#     Bijan Chokoufe <bijan.chokoufe@desy.de>
#     Christian Speckner <cnspeckn@googlemail.com>
#     Marco Sekulla <marco.sekulla@desy.de>
#     Christian Weiss <christian.weiss@desy.de>
#     Felix Braam, Sebastian Schmidt
#
# WHIZARD is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by 
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# WHIZARD is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
########################################################################

## Subdirectories to configure
SUBDIRS = \
  unit_tests \
  functional_tests

## Subdirs to be configured but not built
## In particular, not visited by 'make check'
DIST_SUBDIRS = \
  $(SUBDIRS) \
  extended_tests_NLO \
  extended_tests_shower \
  extended_tests

## Invoke extended tests explicitly by 'make check-extended'
check-extended:
	$(MAKE) -C extended_tests check

## make clean won't be executed automatically in the subdir.
## Note that after 'make distcheck', the Makefile is missing, so this fails,
## that failure should be ignored.

check-extended-shower:
	$(MAKE) -C extended_tests_shower check

check-extended-NLO:
	$(MAKE) -C extended_tests_NLO check

## This won't be executed by default
clean-local:
	-test -f extended_tests/Makefile && $(MAKE) -C extended_tests clean
	-test -f extended_tests_shower/Makefile && $(MAKE) -C extended_tests_shower clean
	-test -f extended_tests_NLO/Makefile && $(MAKE) -C extended_tests_NLO clean
