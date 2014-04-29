#!/bin/sh
### Consider it safer to explicitly mention all files that contain
### email addresses or copyright tags.

OLD_YEAR="Copyright (C) 1999-2013";
NEW_YEAR="Copyright (C) 1999-2014";
OLD_YEAR2="Copyright (C) 2001-2013";
NEW_YEAR2="Copyright (C) 2001-2014";
OLD_ADDRESS="Fabian Bach <fabian.bach@cern.ch>"
NEW_ADDRESS="Fabian Bach <fabian.bach@desy.de>"

OLD_DATE="Feb 03 2014"
NEW_DATE="Mar 03 2014"
OLD_VERSION="2.2.0_beta"
NEW_VERSION="2.2.0"
#OLD_STATUS="PACKAGE_STATUS=\"alpha\""
#NEW_STATUS="PACKAGE_STATUS=\"beta\""
OLD_STATUS="PACKAGE_STATUS=\"beta\""
#NEW_STATUS="PACKAGE_STATUS=\"rc1\""
NEW_STATUS="PACKAGE_STATUS=\"release\""

## We should add an option to add an author here.

## share/doc/manual.tex should be changed manually
## We have to discuss the entries in gamelan/manual
## We have to discuss the entries in src/shower

MAIN_FILES="AUTHORS BUGS Makefile.am README update tests/Makefile.am"
CONFIGURE_FILES="configure.ac src/circe1/configure.ac src/circe2/configure.ac src/omega/configure.ac src/vamp/configure.ac libtool-config/configure.ac"
VERSION_FILES="NEWS"
SCRIPTS_FILES="scripts/Makefile.am scripts/whizard-config.in scripts/whizard-setup.csh.in scripts/whizard-setup.sh.in scripts/libtool-config.sh.in scripts/libtool-relocate.sh.in scripts/whizard-relocate.sh.in"
SHARE_FILES="share/Makefile.am share/doc/Makefile.am share/examples/HERA_DIS.sin share/examples/LEP_cc10.sin share/examples/LEP_higgs.sin share/examples/W-endpoint.sin share/examples/Z-lineshape.sin share/examples/Zprime.sin share/examples/casc_dec.sin share/examples/eeww_polarized.sin share/examples/DrellYanMatchingP.sin share/examples/DrellYanMatchingW.sin share/examples/DrellYanNoMatchingP.sin share/examples/DrellYanNoMatchingW.sin share/examples/EEMatching2P.sin share/examples/EEMatching2W.sin share/examples/EEMatching3P.sin share/examples/EEMatching3W.sin share/examples/EEMatching4P.sin share/examples/EEMatching4W.sin share/examples/EEMatching5P.sin share/examples/EEMatching5W.sin share/examples/EENoMatchingP.sin share/examples/EENoMatchingW.sin share/tests/Makefile.am share/interfaces/Makefile.am"
SRC_FILES="src/Makefile.am src/feynmf/Makefile.am src/hepmc/Makefile.am src/hepmc/HepMCWrap_dummy.f90 src/lhapdf/Makefile.am src/pdf_builtin/Makefile.am src/pdf_builtin/pdf_builtin.f90 src/pythia/Makefile.am src/stdhep/Makefile.am src/whizard-core/Makefile.am src/whizard-core/whizard.nw"
SRC_CIRCE1_FILES="src/circe1/AUTHORS src/circe1/Makefile.am src/circe1/update src/circe1/share/Makefile.am src/circe1/share/doc/Makefile.am src/circe1/src/Makefile.am src/circe1/src/circe1.nw src/circe1/minuit/Makefile.am src/circe1/src/kinds.f90.in src/circe1/src/minuit.nw src/circe1/tools/Makefile.am"
SRC_CIRCE2_FILES="src/circe2/AUTHORS src/circe2/Makefile.am src/circe2/update src/circe2/share/Makefile.am src/circe2/share/doc/Makefile.am src/circe2/src/Makefile.am src/circe2/src/Makefile.ocaml src/circe2/src/circe2.nw src/circe2/src/Makefile.sources src/circe2/tools/Makefile.am src/circe2/src/kinds.f90.in src/circe2/src/postlude.nw"
SRC_GAMELAN_FILES="src/gamelan/Makefile.am src/gamelan/gml.in"
SRC_MISC_FILES="src/misc/Makefile.am src/misc/constants.f90 src/misc/iso_fortran_env_stub.f90 src/misc/kinds.f90.in src/misc/system_dependencies.f90.in"
SRC_MODELS_FILES="src/models/threeshl_bundle/Makefile.am src/models/threeshl_bundle/threeshl_bundle.f90 src/models/threeshl_bundle/threeshl_bundle_lt.f90 src/models/external.Test.f90 src/models/external.Threeshl.f90 src/models/Makefile.am src/models/parameters.2HDM.f90 src/models/parameters.GravTest.f90 src/models/parameters.Littlest.f90 src/models/parameters.Littlest_Eta.f90 src/models/parameters.Littlest_Tpar.f90 src/models/parameters.MSSM.f90 src/models/parameters.MSSM_4.f90 src/models/parameters.MSSM_CKM.f90 src/models/parameters.MSSM_Grav.f90 src/models/parameters.MSSM_Hgg.f90 src/models/parameters.NMSSM.f90 src/models/parameters.NMSSM_CKM.f90 src/models/parameters.NMSSM_Hgg.f90 src/models/parameters.PSSSM.f90 src/models/parameters.QCD.f90 src/models/parameters.QED.f90 src/models/parameters.SM.f90 src/models/parameters.SM_CKM.f90 src/models/parameters.SM_ac.f90 src/models/parameters.SM_ac_CKM.f90 src/models/parameters.SM_rx.f90 src/models/parameters.NoH_rx.f90 src/models/parameters.AltH.f90 src/models/parameters.SSC.f90 src/models/parameters.SM_top.f90 src/models/parameters.SM_top_anom.f90 src/models/parameters.SM_Higgs.f90 src/models/parameters.Simplest.f90 src/models/parameters.Simplest_univ.f90 src/models/parameters.Template.f90 src/models/parameters.Test.f90 src/models/parameters.Threeshl.f90 src/models/parameters.UED.f90 src/models/parameters.Xdim.f90 src/models/parameters.Zprime.f90"
SRC_OMEGA_FILES="src/omega/AUTHORS src/omega/Makefile.am src/omega/update src/omega/share/Makefile.am src/omega/share/doc/Makefile.am src/omega/src/Makefile.am src/omega/src/Makefile.ocaml src/omega/src/Makefile.sources src/omega/bin/Makefile.am src/omega/extensions/Makefile.am src/omega/extensions/people/Makefile.am src/omega/extensions/people/jr/Makefile.am src/omega/extensions/people/jr/f90_SAGT.ml src/omega/extensions/people/jr/f90_SQED.ml src/omega/extensions/people/jr/f90_WZ.ml src/omega/extensions/people/tho/Makefile.am src/omega/extensions/people/tho/f90_O2.ml src/omega/lib/Makefile.am src/omega/models/Makefile.am src/omega/scripts/Makefile.am src/omega/scripts/omega-config.in src/omega/tools/Makefile.am src/omega/tests/MSSM/Makefile.am src/omega/tests/SM/Makefile.am src/omega/tests/people/Makefile.am src/omega/tests/people/jr/Makefile.am src/omega/tests/people/tho/Makefile.am src/omega/tests/parameters_QED.f90 src/omega/tests/parameters_QCD.f90 src/omega/tests/parameters_SM.f90 src/omega/tests/parameters_SYM.f90 src/omega/tests/parameters_SM_top_anom.f90 src/omega/tests/test_openmp.f90 src/omega/tests/tao_random_numbers.f90 src/omega/tests/test_qed_eemm.f90 src/omega/tests/Makefile.am src/omega/tests/benchmark.f90 src/omega/tests/compare.f90 src/omega/tests/color_test_lib.f90 src/omega/tests/compare_lib.f90 src/omega/tests/omega_interface.f90 src/omega/tests/ward.f90 src/omega/tests/ward_lib.f90 src/omega/tests/omega_unit.ml"
SRC_OMEGA_SRC_FILES="src/omega/src/algebra.ml src/omega/src/algebra.mli src/omega/src/bundle.ml src/omega/src/bundle.mli src/omega/src/cache.ml src/omega/src/cache.mli src/omega/src/cascade.ml src/omega/src/cascade.mli src/omega/src/cascade_lexer.mll src/omega/src/cascade_parser.mly src/omega/src/cascade_syntax.ml src/omega/src/cascade_syntax.mli src/omega/src/charges.ml src/omega/src/charges.mli src/omega/src/color.ml src/omega/src/color.mli src/omega/src/colorize.ml src/omega/src/colorize.mli src/omega/src/combinatorics.ml src/omega/src/combinatorics.mli src/omega/src/comphep.ml src/omega/src/comphep.mli src/omega/src/comphep_lexer.mll src/omega/src/comphep_parser.mly src/omega/src/comphep_syntax.ml src/omega/src/comphep_syntax.mli src/omega/src/complex.ml src/omega/src/complex.mli src/omega/src/config.ml.in src/omega/src/config.mli src/omega/src/count.ml src/omega/src/coupling.mli src/omega/src/dAG.ml src/omega/src/dAG.mli src/omega/src/fusion.ml src/omega/src/fusion.mli src/omega/src/linalg.ml src/omega/src/linalg.mli src/omega/src/model.mli src/omega/src/modellib_BSM.ml src/omega/src/modellib_NoH.ml src/omega/src/modellib_NoH.mli src/omega/src/modellib_BSM.mli src/omega/src/modellib_MSSM.ml src/omega/src/modellib_MSSM.mli src/omega/src/modellib_NMSSM.ml src/omega/src/modellib_NMSSM.mli src/omega/src/modellib_PSSSM.ml src/omega/src/modellib_PSSSM.mli src/omega/src/modellib_SM.ml src/omega/src/modellib_SM.mli src/omega/src/modeltools.ml src/omega/src/modeltools.mli src/omega/src/momentum.ml src/omega/src/momentum.mli src/omega/src/oVM.ml src/omega/src/oVM.mli src/omega/src/ogiga.ml src/omega/src/omega.ml src/omega/src/omega.mli src/omega/src/omega_2HDM.ml src/omega/src/omega_CQED.ml src/omega/src/omega_Comphep.ml src/omega/src/omega_GravTest.ml src/omega/src/omega_Littlest.ml src/omega/src/omega_Littlest_Eta.ml src/omega/src/omega_Littlest_Tpar.ml src/omega/src/omega_Littlest_Zprime.ml src/omega/src/omega_MSSM.ml src/omega/src/omega_MSSM_CKM.ml src/omega/src/omega_MSSM_Grav.ml src/omega/src/omega_MSSM_Hgg.ml src/omega/src/omega_NMSSM.ml src/omega/src/omega_NMSSM_CKM.ml src/omega/src/omega_NMSSM_Hgg.ml src/omega/src/omega_PSSSM.ml src/omega/src/omega_Phi3.ml src/omega/src/omega_Phi3h.ml src/omega/src/omega_Phi4.ml src/omega/src/omega_Phi4h.ml src/omega/src/omega_QCD.ml src/omega/src/omega_QED.ml src/omega/src/omega_SM.ml src/omega/src/omega_SM_CKM.ml src/omega/src/omega_SM_Maj.ml src/omega/src/ovm_SM.ml src/omega/src/process.ml src/omega/src/process.mli src/omega/src/thoFilename.ml src/omega/src/thoFilename.mli src/omega/src/omega_SM_Higgs.ml src/omega/src/omega_SM_Rxi.ml src/omega/src/omega_SM_ac.ml src/omega/src/omega_SM_ac_CKM.ml src/omega/src/omega_SM_clones.ml src/omega/src/omega_SM_rx.ml src/omega/src/omega_NoH_rx.ml src/omega/src/omega_AltH.ml src/omega/src/omega_SSC.ml src/omega/src/omega_SM_top.ml src/omega/src/omega_SM_top_anom.ml src/omega/src/omega_SMh.ml src/omega/src/omega_SYM.ml src/omega/src/omega_Simplest.ml src/omega/src/omega_Simplest_univ.ml src/omega/src/omega_Template.ml src/omega/src/omega_Threeshl.ml src/omega/src/omega_Threeshl_nohf.ml src/omega/src/omega_UED.ml src/omega/src/omega_Xdim.ml src/omega/src/omega_Zprime.ml src/omega/src/omega_logo.mp src/omega/src/omega_parameters_tool.nw src/omega/src/omegalib.nw src/omega/src/options.ml src/omega/src/options.mli src/omega/src/partition.ml src/omega/src/partition.mli src/omega/src/phasespace.ml src/omega/src/phasespace.mli src/omega/src/pmap.ml src/omega/src/pmap.mli src/omega/src/powSet.ml src/omega/src/powSet.mli src/omega/src/product.ml src/omega/src/product.mli src/omega/src/progress.ml src/omega/src/progress.mli src/omega/src/rCS.ml src/omega/src/rCS.mli src/omega/src/target.mli src/omega/src/targets.ml src/omega/src/targets.mli src/omega/src/targets_Kmatrix.ml src/omega/src/targets_Kmatrix.mli src/omega/src/test_linalg.ml src/omega/src/thoArray.ml src/omega/src/thoFilename.ml src/omega/src/thoArray.mli src/omega/src/thoGButton.ml src/omega/src/thoGButton.mli src/omega/src/thoGDraw.ml src/omega/src/thoGDraw.mli src/omega/src/thoGMenu.ml src/omega/src/thoGMenu.mli src/omega/src/thoGWindow.ml src/omega/src/thoGWindow.mli src/omega/src/thoList.ml src/omega/src/thoList.mli src/omega/src/thoString.ml src/omega/src/thoString.mli src/omega/src/topology.ml src/omega/src/topology.mli src/omega/src/tree.ml src/omega/src/tree.mli src/omega/src/tree2.ml src/omega/src/tree2.mli src/omega/src/trie.ml src/omega/src/trie.mli src/omega/src/tuple.ml src/omega/src/tuple.mli src/omega/src/vertex.ml src/omega/src/vertex.mli src/omega/src/vertex_lexer.mll src/omega/src/vertex_parser.mly src/omega/src/vertex_syntax.ml src/omega/src/vertex_syntax.mli src/omega/src/whizard.ml src/omega/src/whizard.mli src/omega/src/whizard_tool.ml src/omega/src/kinds.f90.in src/omega/src/constants.f90"   
SRC_PDF_BUILTIN_FILES="src/pdf_builtin/pdf_builtin.f90"
SRC_VAMP_FILES="src/vamp/AUTHORS src/vamp/Makefile.am src/vamp/update src/vamp/share/Makefile.am src/vamp/share/doc/Makefile.am src/vamp/src/Makefile.am src/vamp/src/iso_fortran_env_stub.f90 src/vamp/src/kinds.f90.in"
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

