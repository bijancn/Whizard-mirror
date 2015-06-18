./$1 > ./$1.log
case $? in
0)
  echo ":test-result: PASS"
  ;;
77)
  echo ":test-result: SKIP"
  ;;
*)
  echo ":test-result: FAIL"
  ;;
esac
