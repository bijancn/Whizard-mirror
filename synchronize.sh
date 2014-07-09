#!/bin/sh
### Consider it safer to explicitly mention all files that contain
### email addresses or copyright tags.

OLD_YEAR="Copyright (C) 1999-2013";
NEW_YEAR="Copyright (C) 1999-2014";
OLD_YEAR2="Copyright (C) 2001-2013";
NEW_YEAR2="Copyright (C) 2001-2014";
OLD_ADDRESS="Fabian Bach <fabian.bach@cern.ch>"
NEW_ADDRESS="Fabian Bach <fabian.bach@desy.de>"

OLD_DATE="July 6 2014"
NEW_DATE="Aug 15 2014"
OLD_VERSION="2.2.2"
NEW_VERSION="2.2.3"
#OLD_STATUS="PACKAGE_STATUS=\"alpha\""
#NEW_STATUS="PACKAGE_STATUS=\"beta\""
OLD_STATUS="PACKAGE_STATUS=\"release\""
#NEW_STATUS="PACKAGE_STATUS=\"rc1\""
NEW_STATUS="PACKAGE_STATUS=\"alpha\""

## We should add an option to add an author here.

## share/doc/manual.tex should be changed manually
## We have to discuss the entries in gamelan/manual
## We have to discuss the entries in src/shower

MAIN_FILES="AUTHORS BUGS Makefile.am README update tests/Makefile.am"
CONFIGURE_FILES="configure.ac circe1/configure.ac circe2/configure.ac omega/configure.ac vamp/configure.ac libtool-config/configure.ac"
VERSION_FILES="NEWS omega/NEWS"
SCRIPTS_FILES="scripts/Makefile.am scripts/whizard-config.in scripts/whizard-setup.csh.in scripts/whizard-setup.sh.in scripts/libtool-config.sh.in scripts/libtool-relocate.sh.in scripts/whizard-relocate.sh.in"
SHARE_FILES="share/Makefile.am share/doc/Makefile.am share/examples/HERA_DIS.sin share/examples/LEP_cc10.sin share/examples/LEP_higgs.sin share/examples/W-endpoint.sin share/examples/Z-lineshape.sin share/examples/Zprime.sin share/examples/casc_dec.sin share/examples/eeww_polarized.sin share/examples/DrellYanMatchingP.sin share/examples/DrellYanMatchingW.sin share/examples/DrellYanNoMatchingP.sin share/examples/DrellYanNoMatchingW.sin share/examples/EEMatching2P.sin share/examples/EEMatching2W.sin share/examples/EEMatching3P.sin share/examples/EEMatching3W.sin share/examples/EEMatching4P.sin share/examples/EEMatching4W.sin share/examples/EEMatching5P.sin share/examples/EEMatching5W.sin share/examples/EENoMatchingP.sin share/examples/EENoMatchingW.sin share/tests/Makefile.am share/interfaces/Makefile.am"
SRC_FILES="src/Makefile.am src/feynmf/Makefile.am src/hepmc/Makefile.am src/hepmc/HepMCWrap_dummy.f90 src/lhapdf/Makefile.am src/lhapdf5/Makefile.am src/pdf_builtin/Makefile.am src/pdf_builtin/pdf_builtin.f90 pythia/Makefile.am src/stdhep/Makefile.am src/whizard-core/Makefile.am src/whizard-core/whizard.nw"
SRC_CIRCE1_FILES="circe1/AUTHORS circe1/Makefile.am circe1/update circe1/share/Makefile.am circe1/share/doc/Makefile.am circe1/src/Makefile.am circe1/src/circe1.nw circe1/minuit/Makefile.am circe1/src/kinds.f90.in circe1/src/minuit.nw circe1/tools/Makefile.am"
SRC_CIRCE2_FILES="circe2/AUTHORS circe2/Makefile.am circe2/update circe2/share/Makefile.am circe2/share/doc/Makefile.am circe2/src/Makefile.am circe2/src/Makefile.ocaml circe2/src/circe2.nw circe2/src/Makefile.sources circe2/tools/Makefile.am circe2/src/kinds.f90.in circe2/src/postlude.nw"
SRC_GAMELAN_FILES="src/gamelan/Makefile.am src/gamelan/gml.in"
SRC_MISC_FILES="src/misc/Makefile.am src/misc/constants.f90 src/misc/iso_fortran_env_stub.f90 src/misc/kinds.f90.in src/misc/system_dependencies.f90.in"
SRC_MODELS_FILES="src/models/threeshl_bundle/Makefile.am src/models/threeshl_bundle/threeshl_bundle.f90 src/models/threeshl_bundle/threeshl_bundle_lt.f90 src/models/external.Test.f90 src/models/external.Threeshl.f90 src/models/Makefile.am src/models/parameters.2HDM.f90 src/models/parameters.GravTest.f90 src/models/parameters.Littlest.f90 src/models/parameters.Littlest_Eta.f90 src/models/parameters.Littlest_Tpar.f90 src/models/parameters.MSSM.f90 src/models/parameters.MSSM_4.f90 src/models/parameters.MSSM_CKM.f90 src/models/parameters.MSSM_Grav.f90 src/models/parameters.MSSM_Hgg.f90 src/models/parameters.NMSSM.f90 src/models/parameters.NMSSM_CKM.f90 src/models/parameters.NMSSM_Hgg.f90 src/models/parameters.PSSSM.f90 src/models/parameters.QCD.f90 src/models/parameters.QED.f90 src/models/parameters.SM.f90 src/models/parameters.SM_CKM.f90 src/models/parameters.SM_ac.f90 src/models/parameters.SM_ac_CKM.f90 src/models/parameters.SM_rx.f90 src/models/parameters.NoH_rx.f90 src/models/parameters.AltH.f90 src/models/parameters.SSC.f90 src/models/parameters.SM_top.f90 src/models/parameters.SM_top_anom.f90 src/models/parameters.SM_Higgs.f90 src/models/parameters.Simplest.f90 src/models/parameters.Simplest_univ.f90 src/models/parameters.Template.f90 src/models/parameters.Test.f90 src/models/parameters.Threeshl.f90 src/models/parameters.UED.f90 src/models/parameters.Xdim.f90 src/models/parameters.Zprime.f90"
SRC_OMEGA_FILES="omega/AUTHORS omega/Makefile.am omega/update omega/share/Makefile.am omega/share/doc/Makefile.am omega/src/Makefile.am omega/src/Makefile.ocaml omega/src/Makefile.sources omega/bin/Makefile.am omega/extensions/Makefile.am omega/extensions/people/Makefile.am omega/extensions/people/jr/Makefile.am omega/extensions/people/jr/f90_SAGT.ml omega/extensions/people/jr/f90_SQED.ml omega/extensions/people/jr/f90_WZ.ml omega/extensions/people/tho/Makefile.am omega/extensions/people/tho/f90_O2.ml omega/lib/Makefile.am omega/models/Makefile.am omega/scripts/Makefile.am omega/scripts/omega-config.in omega/tools/Makefile.am omega/tests/MSSM/Makefile.am omega/tests/SM/Makefile.am omega/tests/people/Makefile.am omega/tests/people/jr/Makefile.am omega/tests/people/tho/Makefile.am omega/tests/parameters_QED.f90 omega/tests/parameters_QCD.f90 omega/tests/parameters_SM.f90 omega/tests/parameters_SYM.f90 omega/tests/parameters_SM_top_anom.f90 omega/tests/test_openmp.f90 omega/tests/tao_random_numbers.f90 omega/tests/test_qed_eemm.f90 omega/tests/Makefile.am omega/tests/benchmark.f90 omega/tests/compare.f90 omega/tests/color_test_lib.f90 omega/tests/compare_lib.f90 omega/tests/omega_interface.f90 omega/tests/ward.f90 omega/tests/ward_lib.f90 omega/tests/omega_unit.ml"
SRC_OMEGA_SRC_FILES="omega/src/algebra.ml omega/src/algebra.mli omega/src/bundle.ml omega/src/bundle.mli omega/src/cache.ml omega/src/cache.mli omega/src/cascade.ml omega/src/cascade.mli omega/src/cascade_lexer.mll omega/src/cascade_parser.mly omega/src/cascade_syntax.ml omega/src/cascade_syntax.mli omega/src/charges.ml omega/src/charges.mli omega/src/color.ml omega/src/color.mli omega/src/colorize.ml omega/src/colorize.mli omega/src/combinatorics.ml omega/src/combinatorics.mli omega/src/comphep.ml omega/src/comphep.mli omega/src/comphep_lexer.mll omega/src/comphep_parser.mly omega/src/comphep_syntax.ml omega/src/comphep_syntax.mli omega/src/complex.ml omega/src/complex.mli omega/src/config.ml.in omega/src/config.mli omega/src/count.ml omega/src/coupling.mli omega/src/dAG.ml omega/src/dAG.mli omega/src/fusion.ml omega/src/fusion.mli omega/src/linalg.ml omega/src/linalg.mli omega/src/model.mli omega/src/modellib_BSM.ml omega/src/modellib_NoH.ml omega/src/modellib_NoH.mli omega/src/modellib_BSM.mli omega/src/modellib_MSSM.ml omega/src/modellib_MSSM.mli omega/src/modellib_NMSSM.ml omega/src/modellib_NMSSM.mli omega/src/modellib_PSSSM.ml omega/src/modellib_PSSSM.mli omega/src/modellib_SM.ml omega/src/modellib_SM.mli omega/src/modeltools.ml omega/src/modeltools.mli omega/src/momentum.ml omega/src/momentum.mli omega/src/oVM.ml omega/src/oVM.mli omega/src/ogiga.ml omega/src/omega.ml omega/src/omega.mli omega/src/omega_2HDM.ml omega/src/omega_CQED.ml omega/src/omega_Comphep.ml omega/src/omega_GravTest.ml omega/src/omega_Littlest.ml omega/src/omega_Littlest_Eta.ml omega/src/omega_Littlest_Tpar.ml omega/src/omega_Littlest_Zprime.ml omega/src/omega_MSSM.ml omega/src/omega_MSSM_CKM.ml omega/src/omega_MSSM_Grav.ml omega/src/omega_MSSM_Hgg.ml omega/src/omega_NMSSM.ml omega/src/omega_NMSSM_CKM.ml omega/src/omega_NMSSM_Hgg.ml omega/src/omega_PSSSM.ml omega/src/omega_Phi3.ml omega/src/omega_Phi3h.ml omega/src/omega_Phi4.ml omega/src/omega_Phi4h.ml omega/src/omega_QCD.ml omega/src/omega_QED.ml omega/src/omega_SM.ml omega/src/omega_SM_CKM.ml omega/src/omega_SM_Maj.ml omega/src/ovm_SM.ml omega/src/process.ml omega/src/process.mli omega/src/thoFilename.ml omega/src/thoFilename.mli omega/src/omega_SM_Higgs.ml omega/src/omega_SM_Rxi.ml omega/src/omega_SM_ac.ml omega/src/omega_SM_ac_CKM.ml omega/src/omega_SM_clones.ml omega/src/omega_SM_rx.ml omega/src/omega_NoH_rx.ml omega/src/omega_AltH.ml omega/src/omega_SSC.ml omega/src/omega_SM_top.ml omega/src/omega_SM_top_anom.ml omega/src/omega_SMh.ml omega/src/omega_SYM.ml omega/src/omega_Simplest.ml omega/src/omega_Simplest_univ.ml omega/src/omega_Template.ml omega/src/omega_Threeshl.ml omega/src/omega_Threeshl_nohf.ml omega/src/omega_UED.ml omega/src/omega_Xdim.ml omega/src/omega_Zprime.ml omega/src/omega_logo.mp omega/src/omega_parameters_tool.nw omega/src/omegalib.nw omega/src/options.ml omega/src/options.mli omega/src/partition.ml omega/src/partition.mli omega/src/phasespace.ml omega/src/phasespace.mli omega/src/pmap.ml omega/src/pmap.mli omega/src/powSet.ml omega/src/powSet.mli omega/src/product.ml omega/src/product.mli omega/src/progress.ml omega/src/progress.mli omega/src/rCS.ml omega/src/rCS.mli omega/src/target.mli omega/src/targets.ml omega/src/targets.mli omega/src/targets_Kmatrix.ml omega/src/targets_Kmatrix.mli omega/src/test_linalg.ml omega/src/thoArray.ml omega/src/thoFilename.ml omega/src/thoArray.mli omega/src/thoGButton.ml omega/src/thoGButton.mli omega/src/thoGDraw.ml omega/src/thoGDraw.mli omega/src/thoGMenu.ml omega/src/thoGMenu.mli omega/src/thoGWindow.ml omega/src/thoGWindow.mli omega/src/thoList.ml omega/src/thoList.mli omega/src/thoString.ml omega/src/thoString.mli omega/src/topology.ml omega/src/topology.mli omega/src/tree.ml omega/src/tree.mli omega/src/tree2.ml omega/src/tree2.mli omega/src/trie.ml omega/src/trie.mli omega/src/tuple.ml omega/src/tuple.mli omega/src/vertex.ml omega/src/vertex.mli omega/src/vertex_lexer.mll omega/src/vertex_parser.mly omega/src/vertex_syntax.ml omega/src/vertex_syntax.mli omega/src/whizard.ml omega/src/whizard.mli omega/src/whizard_tool.ml omega/src/kinds.f90.in omega/src/constants.f90"   
SRC_PDF_BUILTIN_FILES="src/pdf_builtin/pdf_builtin.f90"
SRC_VAMP_FILES="vamp/AUTHORS vamp/Makefile.am vamp/update vamp/share/Makefile.am vamp/share/doc/Makefile.am vamp/src/Makefile.am vamp/src/iso_fortran_env_stub.f90 vamp/src/kinds.f90.in"
LIBTOOL_CONFIG_FILES="libtool-config/Makefile.am libtool-config/update"
FILES="$MAIN_FILES $CONFIGURE_FILES $VERSION_FILES $SHARE_FILES $SRC_OMEGA_FILES $SCRIPTS_FILES $SRC_FILES $SRC_CIRCE1_FILES $SRC_CIRCE2_FILES $SRC_GAMELAN_FILES $SRC_PDF_BUILTIN_FILES $SRC_VAMP_FILES $SRC_MISC_FILES $SRC_MODELS_FILES $SRC_OMEGA_SRC_FILES $LIBTOOL_CONFIG_FILES"

for f in $FILES; do
sed -e "s/$OLD_YEAR/$NEW_YEAR/g" -e "s/$OLD_YEAR2/$NEW_YEAR2/g" -e "s/$OLD_ADDRESS/$NEW_ADDRESS/g" $f > $f.tmp;
cp -f $f.tmp $f; 
rm -f $f.tmp;
done 

CHANGE_FILES="$CONFIGURE_FILES $VERSION_FILES"
for f in $CHANGE_FILES; do
sed -e "s/$OLD_DATE/$NEW_DATE/g" -e "s/$OLD_VERSION/$NEW_VERSION/g" -e "s/$OLD_STATUS/$NEW_STATUS/g" $f > $f.tmp;
cp -f $f.tmp $f; 
rm -f $f.tmp;
done 

