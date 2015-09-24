#! /bin/sh
########################################################################
if test -f OCAML_FLAG; then
  if test ! -f QUAD_FLAG; then
      exec ./test_wrapper.sh @name@ check_generation </dev/null >/dev/null 2>&1
  else
      echo "|=============================================================================|"
      echo "Detected quadruple precision: test skipped for now"
      exit 77
  fi
else
    echo "|=============================================================================|"
    echo "No OCaml for testing tools available, test skipped"
    exit 77
fi
