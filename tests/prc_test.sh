#!/bin/sh
### Check WHIZARD/O'Mega with externally linked LHAPDF library
echo "Running script $0"
exec ./run_whizard_ut.sh @script@ --check prc_test
