#!/bin/sh
### Check an NLO process to verify the OpenLoops interface
echo "Running script $0"
if test -f OCAML_FLAG -a -f OPENLOOPS_FLAG; then
    name=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    echo "Contents of ${name}_p1.debug:" >> $name.log
    cat ${name}_p1.debug >> $name.log
    sed -i '/Loading library: /d' $name.log
    diff ref-output/$name.ref $name.log
else
    echo "|=============================================================================|"
    echo "No O'Mega or OpenLoops matrix elements available, test skipped"
    exit 77
fi
