#!/bin/sh
# time ~12 min
echo "Running script $0"
if test -f OCAML_FLAG -a -f PYTHIA6_FLAG -a -f PYTHON_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    @python_bin@ @share_dir@/check-debug-output.py ${s}_WHIZARD 1>&2 &>> $s.run.log
    @python_bin@ @share_dir@/check-debug-output.py ${s}_PYTHIA6 1>&2 &>> $s.run.log
  else
    echo "|=============================================================================|"
    echo "O'Mega, PYTHIA6 and/or Python disabled/not available, test skipped"
    exit 77
fi
