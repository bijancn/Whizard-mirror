#!/bin/sh
### Check WHIZARD module sm_physics
echo "Running script $0"
exec ./run_whizard_ut.sh @script@ --check sm_qcd
