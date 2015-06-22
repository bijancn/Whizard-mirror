import re, sys

re_e = re.compile("p\(0:3\) = *([0-9]+\.[0-9]+)")
energy = lambda line: float(re_e.search(line).group(1))
test = sys.argv[1]
filename = test + '.debug'
inc_energy = 0.0
out_energy = 0.0
line_no = 0
delta = 10.0**-5
valid = True

print 'Start file'
print (3 * '{:<20s}').format('Line-Nr', 'Incoming-Energy', 'Outgoing-Energy')
with open(filename, 'r') as infile:
  for line in infile:
    line_no += 1
    if 'incoming momenta' in line:
      inc_energy = energy(line)
    elif 'outgoing momenta' in line:
      out_energy = energy(line)
      if abs(out_energy - inc_energy) / max([out_energy, inc_energy]) > delta:
        print ("{:<20d}" + 2 *"{:<20.10f}").format(line_no, inc_energy, out_energy)
        valid = False
print 'End file'
returncode = 0 if valid else 1
sys.exit(returncode)
