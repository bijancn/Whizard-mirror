#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
if test -f HEPMC_FLAG -a -f OCAML_FLAG; then
    name=`basename @script@`
    ./run_whizard.sh @script@ --no-logging --model QCD
    echo "Contents of ${name}a.weights.dat" >> $name.log
    cat ${name}a.weights.dat | sed -e 's/..E/XXE/g' >> $name.log
    echo "Contents of ${name}b.weights.dat" >> $name.log
    cat ${name}b.weights.dat | sed -e 's/..E/XXE/g' >> $name.log
    echo "Contents of ${name}c.weights.dat" >> $name.log
    cat ${name}c.weights.dat | sed -e 's/..E/XXE/g' >> $name.log
    diff ref-output/`basename @script@`.ref `basename @script@`.log
else
    echo "|=============================================================================|"
    echo "HepMC disabled or no O'Mega matrix elements available, test skipped"
    exit 77
fi
    
