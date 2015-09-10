#!/bin/sh
### Check WHIZARD for a simple NLO process with dummy virtual matrix-elements and combined integration
echo "Running script $0"
if test -f OCAML_FLAG; then
    name=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    cat $name.log | sed -e 's/Reading model file.*/Reading model file SM_rad.mdl/' > $name.log.tmp
    mv $name.log.tmp $name.log
    diff ref-output/`basename @script@`.ref `basename @script@`.log
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi
