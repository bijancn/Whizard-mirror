#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
if test -f OCAML_FLAG -a -f STDHEP_FLAG; then
    rm -f @script@_lib.* @script@_p?.*
    rm -f default_prclib.*
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging --model SM
    echo "STHDHEP file contents:" > ${s}_hep.log
    ./stdhep_rd ${s}_p.hep 3 >> ${s}_hep.log
    mv ${s}_hep.log ${s}_hep.log.tmp
    cat ${s}_hep.log.tmp | sed -e 's/total blocks.*/total blocks: [...]/' -e 's/WHIZARD 2.*/WHIZARD [version]/' -e 's/date: .*/date: [...]/' > ${s}_hep.log
    diff ref-output/$s.ref ${s}_hep.log
else
    echo "|=============================================================================|"
    echo "No StdHEP available, test skipped"
    exit 77
fi


