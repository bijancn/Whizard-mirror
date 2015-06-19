#!/bin/sh
# TODO: (bcn 2015-06-18) description
# TODO: (bcn 2015-06-18) why do we have no OPENLOOPS_FLAG ?
echo "Running script $0"
if test -f OCAML_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    diff ref-output/$s.ref $s.log
  else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available"
    exit 77
fi
