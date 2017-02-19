import sys
import re

line = sys.stdin.readline()

curPass = ""
passTime = ""
nodeCnt = ""
nodeTime = ""

stats = []

while line:
  print line.rstrip()
  line = sys.stdin.readline()

  m1 = re.search('(\w[\w\s]+) took (.+) ms', line)
  if m1:
    if (m1.group(1) == 'Count Unique Nodes'):
      nodeTime = m1.group(2)
    else:
      curPass = m1.group(1)
      passTime = m1.group(2)

  m2 = re.search('Number of unique nodes = (\d+)', line)
  if m2:
    nodeCnt = m2.group(1)
    stats.append([curPass, passTime, nodeCnt, nodeTime])

for stat in stats:
  print ','.join(stat)
