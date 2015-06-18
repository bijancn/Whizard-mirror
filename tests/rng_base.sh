#!/bin/sh
### Check WHIZARD module rng_base
echo "Running script $0"
exec ./run_whizard_ut.sh @script@ --check rng_base
