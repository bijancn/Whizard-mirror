#!/bin/sh
### Check WHIZARD process library setup
echo "Running script $0"
if test -f OCAML_FLAG; then
    exec ./run_whizard_ut.sh --check prc_omega
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi
    
