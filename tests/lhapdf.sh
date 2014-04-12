#!/bin/sh
### Check WHIZARD/O'Mega with externally linked LHAPDF library
echo "Running script $0"
if test -f OCAML_FLAG -a -f LHAPDF_FLAG; then
    rm -f @script@_lib.* @script@_p?.*
    rm -f default_prclib.*
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging --model SM
    echo "LHEF file contents:" >> $s.log
    for i in 1 2 3 
    do
	cat ${s}_1_p_$i.lhe | sed -e 's/^  <generator_version>.*$/[...]/' >> $s.log
    done
    diff ref-output/$s.ref $s.log
else
    echo "|=============================================================================|"
    echo "Either no O'Mega matrix elements or no LHAPDF available, test skipped"
    exit 77
fi
