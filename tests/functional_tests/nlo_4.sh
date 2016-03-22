#!/bin/sh
### Check WHIZARD POWHEG matching with dummy virtual matrix-elements
echo "Running script $0"
if test -f OCAML_FLAG; then
    (time -p ( {
    name=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    echo "Contents of ${name}_p1.debug:" >> $name.log
    cat ${name}_p1.debug >> $name.log
    diff ref-output/$name.ref $name.log
    } 2>&3) ) 3>&2 2>$name.timing
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi
