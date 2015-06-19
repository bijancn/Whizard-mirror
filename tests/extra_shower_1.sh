#!/bin/sh
# TODO: (bcn 2015-06-18) description
echo "Running script $0"
if test -f OCAML_FLAG -a -f PYTHIA6_FLAG -a -f PYTHON_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    diff ref-output/$s.ref $s.log
  else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available and/or PYTHIA6 disabled, test skipped"
    exit 77
fi
