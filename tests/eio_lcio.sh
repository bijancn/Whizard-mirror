#!/bin/sh
### Check WHIZARD eio_lcio module setup
echo "Running script $0"
if test -f LCIO_FLAG; then
    exec ./run_whizard_ut.sh @script@ --check eio_lcio
else
    echo "|=============================================================================|"
    echo "No LCIO available, test skipped"
    exit 77
fi
