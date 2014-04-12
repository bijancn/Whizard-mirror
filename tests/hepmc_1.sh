#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
if test -f OCAML_FLAG -a -f HEPMC_FLAG; then
    rm -f @script@_lib.* @script@_p?.*
    rm -f default_prclib.*
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging --model QED
    # HepMC file contents depend on the specific of the installation
    # echo "HepMC file contents:" >> $s.log
    # cat ${s}_p.hepmc >> $s.log
    diff ref-output/$s.ref $s.log
else
    echo "|=============================================================================|"
    echo "No HepMC available, test skipped"
    exit 77
fi

