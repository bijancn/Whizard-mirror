#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
if test -f OCAML_FLAG; then
    rm -f @script@_lib.* @script@_p?.*
    rm -f default_prclib.*
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging --model QCD
    # echo "STDHEP file contents:" >> $s.log
    # cat ${s}_p.stdhep_up >> $s.log
    diff ref-output/$s.ref $s.log
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi

