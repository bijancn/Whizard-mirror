## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in
#
# Copyright (C) 1999-2016 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
#     Bijan Chokoufe <bijan.chokoufe@desy.de>
#     Christian Speckner <cnspeckn@googlemail.com>
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

## The files in this directory end up in an auxiliary libtool library.
noinst_LTLIBRARIES = libtauola_interface.la
libtauola_interface_la_SOURCES = \
   hepev4_aux.f90 \
   tauola_interface.f90

AM_FCFLAGS =

########################################################################
## Default Fortran compiler options

## Profiling
if FC_USE_PROFILING
AM_FCFLAGS += $(FCFLAGS_PROFILING)
endif

## OpenMP
if FC_USE_OPENMP
AM_FCFLAGS += $(FCFLAGS_OPENMP)
endif

########################################################################
## Explicit dependencies

tauola_interface.lo: ../events/hep_common.$(FC_MODULE_EXT)
tauola_interface.lo: hepev4_aux.lo
tauola_interface.lo: ../variables/variables.$(FC_MODULE_EXT)
tauola_interface.lo: ../qft/model_data.$(FC_MODULE_EXT)
tauola_interface.lo: ../basics/kinds.$(FC_MODULE_EXT)
tauola_interface.lo: ../basics/iso_varying_string.$(FC_MODULE_EXT)
tauola_interface.lo: ../system/diagnostics.$(FC_MODULE_EXT)

AM_FCFLAGS += -I../basics -I../utilities -I../system -I../expr_base -I../combinatorics -I../parsing -I../physics -I../qft -I../types -I../fastjet -I../particles -I../events -I../variables

MODULES= \
  tauola_interface.$(FC_MODULE_EXT) \
  hepev4_aux.$(FC_MODULE_EXT)

########################################################################
## Non-standard cleanup tasks

## Remove backup files
maintainer-clean-local:
	-rm -f *~

## Remove module files
clean-local:
	-rm -f $(MODULES)
if FC_SUBMODULES
	-rm -f *.smod
endif
