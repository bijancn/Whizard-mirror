#!/bin/sh
# time: ~4 min
echo "Running script $0"
if test -f OCAML_FLAG -a -f PYTHIA6_FLAG -a -f PYTHON_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    rm -f $s.integration
    @python_bin@ @share_dir@/compare-integrals.py $s \
      @share_dir@/extra_integration_results.dat &>> $s.run.log
    @python_bin@ @share_dir@/compare-histograms.py $s &>> $s.run.log
    @python_bin@ @share_dir@/check-debug-output.py $s &>> $s.run.log
  else
    echo "|=============================================================================|"
    echo "O'Mega, PYTHIA6 and/or Python disabled/not available, test skipped"
    exit 77
fi
