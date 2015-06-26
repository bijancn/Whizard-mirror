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

WHIZARD_DRIVER = run_whizard.sh

TESTS_EXTENDED = \
    sm_ext.run \
    mssm_ext.run \
    nmssm_ext.run
XFAIL_TESTS_EXTENDED =

TESTS_REQ_GAMELAN = \
    $(EXTRACHECK_SHOWER_RUN)

TEST_DRIVERS_RUN = \
    $(TESTS_EXTENDED)

TEST_DRIVERS_SH = $(TEST_DRIVERS_RUN:.run=.sh)

########################################################################

TESTS = 
XFAIL_TESTS =
TESTS_SRC =

if EXT_NUM_CHECKS
TESTS += $(TESTS_EXTENDED)
XFAIL_TESTS += $(XFAIL_TESTS_EXTENDED)
endif EXT_NUM_CHECKS

mssm_ext.run: $(EXTENDED_MSSM_TEST_DRIVERS)
	echo '#! /bin/sh' >$@
	for driver in $(EXTENDED_MSSM_TEST_DRIVERS); do \
	  echo ./$$driver; \
	done >>$@
	chmod +x $@

nmssm_ext.run: $(EXTENDED_NMSSM_TEST_DRIVERS)
	echo '#! /bin/sh' >$@
	for driver in $(EXTENDED_NMSSM_TEST_DRIVERS); do \
	  echo ./$$driver; \
	done >>$@
	chmod +x $@

EXTENDED_MSSM_TEST_DRIVERS = \
    mssm_ext-ee.run mssm_ext-ee2.run \
    mssm_ext-en.run mssm_ext-tn.run \
    mssm_ext-uu.run mssm_ext-uu2.run mssm_ext-uuckm.run \
    mssm_ext-dd.run mssm_ext-dd2.run mssm_ext-ddckm.run \
    mssm_ext-bb.run mssm_ext-bt.run mssm_ext-tt.run \
    mssm_ext-ug.run mssm_ext-dg.run \
    mssm_ext-aa.run mssm_ext-wa.run mssm_ext-za.run \
    mssm_ext-ww.run mssm_ext-wz.run mssm_ext-zz.run \
    mssm_ext-gg.run mssm_ext-ga.run mssm_ext-gw.run mssm_ext-gz.run

EXTENDED_NMSSM_TEST_DRIVERS = \
    nmssm_ext-aa.run  nmssm_ext-bb1.run nmssm_ext-bb2.run \
    nmssm_ext-bt.run  nmssm_ext-dd1.run \
    nmssm_ext-dd2.run nmssm_ext-ee1.run \
    nmssm_ext-ee2.run nmssm_ext-en.run  \
    nmssm_ext-ga.run  nmssm_ext-gg.run  \
    nmssm_ext-gw.run  nmssm_ext-gz.run  \
    nmssm_ext-qg.run  nmssm_ext-tn.run  \
    nmssm_ext-tt1.run nmssm_ext-tt2.run \
    nmssm_ext-uu1.run nmssm_ext-uu2.run \
    nmssm_ext-wa.run  nmssm_ext-ww1.run nmssm_ext-ww2.run \
    nmssm_ext-wz.run  nmssm_ext-za.run

EXTENDED_MSSM_TEST_DRIVERS_SH = $(EXTENDED_MSSM_TEST_DRIVERS:.run=.sh)
EXTENDED_NMSSM_TEST_DRIVERS_SH = $(EXTENDED_NMSSM_TEST_DRIVERS:.run=.sh)

EXTRA_DIST = $(TEST_DRIVERS_SH) \
    $(EXTENDED_MSSM_TEST_DRIVERS_SH) \
    $(EXTENDED_NMSSM_TEST_DRIVERS_SH) \
    $(TESTS_SRC)

########################################################################

EXTRA_DIST += extracheck_combiner.sh produce_trs.sh

.run.run.extra.trs:
	$(SHELL) $(srcdir)/produce_trs.sh $< >| $@

clean-extracheck_results:
	rm -f *.extra.trs
.PHONY: clean-extracheck_results

EXTRACHECK_SHOWER_RUN = extra_shower_1.run extra_shower_2.run extra_shower_3.run
EXTRACHECK_SHOWER_SH = $(EXTRACHECK_SHOWER_RUN:.run=.sh)
EXTRACHECK_SHOWER_TRS = $(EXTRACHECK_SHOWER_RUN:.run=.run.extra.trs)
.SECONDARY: $(EXTRACHECK_SHOWER_RUN)
extracheck_shower: $(BUILT_SOURCES) $(EXTRACHECK_SHOWER_TRS)
	$(SHELL) $(srcdir)/extracheck_combiner.sh "extra_shower_*.run.extra.trs"

EXTRACHECK_NLO_RUN = extra_NLO_1.run
EXTRACHECK_NLO_SH = $(EXTRACHECK_NLO_RUN:.run=.sh)
EXTRACHECK_NLO_TRS = $(EXTRACHECK_NLO_RUN:.run=.run.extra.trs)
.SECONDARY: $(EXTRACHECK_NLO_RUN)
extracheck_NLO: $(BUILT_SOURCES) $(EXTRACHECK_NLO_TRS)
	$(SHELL) $(srcdir)/extracheck_combiner.sh "extra_NLO_*.run.extra.trs"

########################################################################

# VPATH = $(top_builddir)/share/tests:$(top_srcdir)/share/tests:$(srcdir)
VPATH = $(srcdir)

SUFFIXES = .sh .run .run.extra.trs

.sh.run:
	@rm -f $@
	@if test -f $(top_builddir)/share/tests/$*.sin; then \
	  $(SED) 's|@script@|$(top_builddir)/share/tests/$*|g' $< > $@; \
	elif test -f $(top_srcdir)/share/tests/$*.sin; then \
	  $(SED) 's|@script@|$(top_srcdir)/share/tests/$*|g' $< > $@; \
	fi
	$(SED) -i 's|@python_bin@|$(PYTHON_BIN)|g' $@
	$(SED) -i 's|@share_dir@|$(top_srcdir)/share/tests/|g' $@
	chmod +x $@

if MPOST_AVAILABLE
$(TESTS_REQ_GAMELAN): gamelan.sty
$(UNIT_TESTS_REQ_GAMELAN): gamelan.sty
gamelan.sty: $(top_builddir)/src/gamelan/gamelan.sty
	cp $< $@

$(top_builddir)/src/gamelan/gamelan.sty:
	$(MAKE) -C $(top_builddir)/src/gamelan gamelan.sty
endif

BUILT_SOURCES = \
    TESTFLAG  \
    STDHEP_FLAG \
    HEPMC_FLAG \
    LCIO_FLAG \
    FASTJET_FLAG \
    LHAPDF5_FLAG \
    LHAPDF6_FLAG \
    GAMELAN_FLAG \
    EVENT_ANALYSIS_FLAG \
    OCAML_FLAG \
    PYTHON_FLAG \
    PYTHIA6_FLAG \
    STATIC_FLAG \
    ref-output

# If this file is found in the working directory, WHIZARD
# will use the paths for the uninstalled version (source/build tree),
# otherwise it uses the installed version
TESTFLAG:
	touch $@

STDHEP_FLAG:
if STDHEP_AVAILABLE
	touch $@
endif

FASTJET_FLAG:
if FASTJET_AVAILABLE
	touch $@
endif

HEPMC_FLAG:
if HEPMC_AVAILABLE
	touch $@
endif

LCIO_FLAG:
if LCIO_AVAILABLE
	touch $@
endif

LHAPDF5_FLAG:
if LHAPDF5_AVAILABLE
	touch $@
endif

LHAPDF6_FLAG:
if LHAPDF6_AVAILABLE
	touch $@
endif

GAMELAN_FLAG:
if MPOST_AVAILABLE
	touch $@
endif

OCAML_FLAG:
if OCAML_AVAILABLE
	touch $@
endif

PYTHON_FLAG:
if PYTHON_AVAILABLE
	touch $@
endif

PYTHIA6_FLAG:
if PYTHIA6_AVAILABLE
	touch $@
endif

EVENT_ANALYSIS_FLAG:
if EVENT_ANALYSIS_AVAILABLE
	touch $@
endif

STATIC_FLAG:
if STATIC_AVAILABLE
	touch $@
endif

# The reference output files are in the source directory.  Copy them here.
if FC_EXT
ref-output: $(top_srcdir)/share/tests/ref-output 
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done
	for f in $</../ref-output-ext/*.ref; do cp $$f $@; done
else
ref-output: $(top_srcdir)/share/tests/ref-output 
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done
	for f in $</../ref-output-double/*.ref; do cp $$f $@; done
endif

## installcheck runs the test scripts with the TESTFLAG removed.
installcheck-local: notestflag check-am
notestflag:
	rm -f TESTFLAG
.PHONY: notestflag

## Remove generated files
clean-local:
	rm -f gamelan.sty TEST.mdl 
	rm -f TESTFLAG GAMELAN_FLAG STDHEP_FLAG 
	rm -f OCAML_FLAG PYTHON_FLAG FASTJET_FLAG HEPMC_FLAG LCIO_FLAG 
	rm -f EVENT_ANALYSIS_FLAG PYTHIA6_FLAG LHAPDF5_FLAG
	rm -f LHAPDF6_FLAG STATIC_FLAG static_1.exe
	rm -f *.run *.log slha_test.out
	rm -f core* stdhep_rd
	rm -f *.f90 *.c *.$(FC_MODULE_EXT) *.o *.la
	rm -f *.makefile
	rm -rf err-output
	rm -rf ref-output
	rm -f *.sin *.hbc 
	rm -f *.phs *.vg *.vgb *.evt *.evx *.lhe *.hepmc *.dat *.debug 
	rm -f *.tmp *.hepevt *.hepevt.verb *.lha *.lha.verb *.slcio
	rm -f prc_omega_diags_1_p_i1_diags.out prc_omega_diags_1_p_i1_diags.toc
	rm -f *.hep *.up.hep *.[1-9] *.[1-9][0-9] *.[1-9][0-9][0-9] 
	rm -f *.tex *.mp *.mpx *.t[1-9] *.t[1-9][0-9] *.t[1-9][0-9][0-9] 
	rm -f *.ltp *.aux *.dvi *.ps *.pdf so_test.*
	rm -f *.tbl sps1ap_decays.slha bar structure_6[a-b].out 
	rm -f slhaspectrum.in suspect2.out suspect2_lha.out 
	rm -f susyhit.in susyhit_slha.out suspect2_lha.in
	rm -f *.$(WT_OMEGA_CACHE_SUFFIX)
	rm -f *.run.extra.trs
	rm -f *.olc *.olp *.olp_parameters
	rm -rf stability_log

## Remove backup files
maintainer-clean-local: maintainer-clean-fc
	-rm -f *~
.PHONY: maintainer-clean-local
