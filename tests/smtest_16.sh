#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
if test -f OCAML_FLAG; then
    if test "$FC_VENDOR" = "gfortran" -a "$FC_VERSION" \< "4.7.4"; then
	echo "|=============================================================================|"
	echo "Avoiding bug in gfortran < 4.7.4, test skipped"
	exit 77
    else
	name=`basename @script@`
	./run_whizard.sh @script@ --no-logging
	cat $name.log | sed -e 's/Reading model file.*/Reading model file SM_rad.mdl/' > $name.log.tmp
	mv $name.log.tmp $name.log
	diff ref-output/`basename @script@`.ref `basename @script@`.log
    fi
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi
