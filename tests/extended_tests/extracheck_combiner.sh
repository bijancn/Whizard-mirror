tests="$1"
echo $tests
echo
for t in $1; do
  result=`grep ":test-result:" $t | sed 's/:test-result://'`
  test=$t
  test=`echo $t | sed 's/.run.extra.trs//'`
  echo '   ' $test '   ' $result
done
