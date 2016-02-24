#!/bin/sh
echo "Running script $0"
if test -f OCAML_FLAG; then
    ./run_whizard.sh @script@ --no-logging --no-model
    script=`basename @script@`
    echo "Contents of ${script}_p1.hepevt:" >> $script.log
    cat ${script}_p1.hepevt >> $script.log
    echo "STDHEP EV4 version:" >> $script.log
    ./stdhep_rd polarized_1_p1.ev4.hep 1 >> $script.log
    diff ref-output/$script.ref $script.log
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi
