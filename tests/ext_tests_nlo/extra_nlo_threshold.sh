#!/bin/sh
# time ~
echo "Running script $0"
if test -f OCAML_FLAG -a -f OPENLOOPS_FLAG; then
    s=`basename @script@`
    (time -p (
      {
        ./run_whizard.sh @script@ --no-logging --debug threshold
        rc=$?; if [ $rc != 0 ]; then exit $rc; fi
      } 2>&3
    ) ) 3>&2 2>${s}.timing
  else
    echo "|=============================================================================|"
    echo "No O'Mega and/or OpenLoops matrix elements available"
    exit 77
fi
