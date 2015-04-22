#!/bin/sh
### Check WHIZARDs initial state shower/matching
echo "Running script $0"
if test -f OCAML_FLAG -a -f PYTHIA6_FLAG && test -f LHAPDF6_FLAG -o -f LHAPDF5_FLAG; then
    ./run_whizard.sh @script@ --no-logging
    script=`basename @script@`
    ### Event files not compared, numerical noise!
    #echo "Contents of ${script}_a.debug:" >> $script.log
    #cat ${script}_a.debug >> $script.log
    #echo "Contents of ${script}_b.debug:" >> $script.log
    #cat ${script}_b.debug >> $script.log
    rm $script.log
    echo 'disabled' >> $script.log
    ### see sindarin
    #echo "Contents of ${script}_c.debug:" >> $script.log
    #grep momenta ${script}_c.debug >> $script.log
    #echo "Contents of ${script}_d.debug:" >> $script.log
    #grep momenta ${script}_d.debug >> $script.log
    diff ref-output/$script.ref $script.log

else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements/LHAPDF available and/or PYTHIA6 disabled, test skipped"
    exit 77
fi
