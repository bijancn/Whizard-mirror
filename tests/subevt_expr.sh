#!/bin/sh
### Check WHIZARD event setup
echo "Running script $0"
exec ./run_whizard_ut.sh @script@ --check subevt_expr

