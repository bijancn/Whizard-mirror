#!/bin/sh
### Check WHIZARD module mci_base
echo "Running script $0"
exec ./run_whizard_ut.sh @script@ --check mci_base
