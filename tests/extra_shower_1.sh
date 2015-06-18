#!/bin/sh
### Describe what is looked at
echo "Running script $0"
if test -f OCAML_FLAG -a -f PYTHIA6_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    diff ref-output/$s.ref $s.log
  else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available and/or PYTHIA6 disabled, test skipped"
    exit 77
fi
