#!/bin/sh
### Check WHIZARD module sf_beam_events
echo "Running script $0"
exec ./run_whizard_ut.sh @script@ --check sf_beam_events
