#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
if test -f GOSAM_FLAG; then
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
else
    echo "|=============================================================================|"
    echo "No GoSam libraries available, test skipped"
    exit 77
fi
