#!/bin/sh
# We look for mismatches in incoming vs outgoing energy via
# check-debug-output.py that indicate final state particles mistagged as
# resonant.
echo "Running script $0"
if test -f OCAML_FLAG -a -f PYTHIA6_FLAG -a -f PYTHON_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    @python_bin@ @src_dir@/check-debug-output.py $s 1>&2 >> $s.run.log
  else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available and/or PYTHIA6 disabled, test skipped"
    exit 77
fi
