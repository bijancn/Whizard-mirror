import re, sys
from math import sqrt

delta = 1E-12
re_nums = re.compile(" +\S+ +(\d+\.\d+E\S*)  (\d*\.\d+E\S*)")
re_bin = re.compile(" +(\d*\.\d+E\S*)")
bin_midpoint = lambda line: float(re_bin.search(line).group(1))
numbers = lambda line: (float(re_nums.search(line).group(1)),
                        float(re_nums.search(line).group(2)))

def pull(line, ref_line):
  value, error = numbers(line)
  ref_value, ref_error = numbers(ref_line)
  error_sum = sqrt (error**2 + ref_error**2)
  diff = abs (value - ref_value)
  if abs (error_sum) > delta:
    result = diff / error_sum
  else:
    result = 0.0 if diff < delta else 10.0
  return result

process = sys.argv[1]
filename = process + '_hist.dat'
reference = 'ref-output/' + process + '.ref'
pull_max = 0.0

# In Python 2.7 we could write this in one line
with open(filename, 'r') as infile:
  with open(reference, 'r') as ref_file:
    for line, ref_line in zip(infile, ref_file):
      if '#' not in line and len(line) > 1:
        this_pull = pull(line, ref_line)
        pull_max = max(pull_max, this_pull)
        print (2 * '{:<30.10f}').format(bin_midpoint(line), this_pull)
      elif 'Underflow' in line:
        break
print 'max pull:', pull_max
returncode = 0 if pull_max < 3 else 1
sys.exit(returncode)
