#!/bin/sh
### Check WHIZARD: process library handling
echo "Running script $0"
if test -f OCAML_FLAG; then
    rm -f SM_tt_threshold.grid
    ./run_whizard.sh @script@ --no-logging
    diff ref-output/`basename @script@`.ref `basename @script@`.log
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi