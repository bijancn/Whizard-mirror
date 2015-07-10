import re, sys

re = re.compile("p\(0:3\) = *([0-9]+\.[0-9]+)")
def weight(line):
  try:
    return float(re.search(line).group(1))
  except AttributeError:
    print 'Could not find number in this line:'
    print line
    return 0.0

test = sys.argv[1]
filename = test + '.hepmc'
line_no = 0
valid = True

print 'Start file ' + filename
print (4 * '{:<20s}').format('Line-Nr', 'BeamRemnant-Energy', 'Outgoing-Energy',
    'BeamRemnant+Outgoing-Energy')
with open(filename, 'r') as infile:
  for line in infile:
    line_no += 1
    if 'beam remnant momenta' in line:
      br_energy = energy(line)
    elif 'outgoing momenta' in line:
      out_energy = energy(line)
      if not nearly_equal(sqrts, out_energy + br_energy):
        print ("{:<20d}" + 3 *"{:<20.10f}").format(line_no, br_energy,
            out_energy, out_energy + br_energy)
        valid = False
print 'Number of lines', line_no
print 'End file ' + filename
returncode = 0 if valid else 1
sys.exit(returncode)
