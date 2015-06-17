#!/bin/sh
### Check WHIZARD process-stack setup
echo "Running script $0"
exec ./run_whizard_ut.sh @script@ --check process_stacks
