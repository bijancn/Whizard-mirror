#!/bin/sh
### Check WHIZARD module rng_tao
echo "Running script $0"
exec ./run_whizard_ut.sh @script@ --check rng_tao
