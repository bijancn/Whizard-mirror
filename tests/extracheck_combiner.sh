tests=$1
echo
for t in $tests; do
  result=`grep ":test-result:" $t | sed 's/:test-result://'`
  test=$t
  test=`echo $t | sed 's/.run.extra.trs//'`
  echo '   ' $test '   ' $result
done
