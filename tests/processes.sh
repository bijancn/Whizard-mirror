#!/bin/sh
### Check WHIZARD process setup
echo "Running script $0"
exec ./run_whizard.sh @script@ --check processes
