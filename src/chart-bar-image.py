import re
import sys
import matplotlib.pyplot as plt

chart_path = sys.argv[1]
image_fmt = sys.argv[2]

header = None
xs = []
records = []
for line in sys.stdin:
    if line[len(line) - 1] == "\n":
        line = line[0 : len(line) - 1]
    cols = line.split("\t")
    if not header:
        cols.pop(0)
        header = cols
    else:
        x = cols.pop(0)
        #if re.compile("/\A[-+]?[0-9]+(\.[0-9]*)?\Z/").match(x):
        #x = float(x)
        xs.append(x)
        records.append(cols)

for i in range(len(header)):
    h = header[i]
    x = []
    y = []
    for j in range(len(records)):
        x.append(xs[j])
        y.append(float(records[j][i]))
    plt.bar(x, y)

plt.savefig(chart_path, format=image_fmt)

#print(header)
#print(records)
#sys.stdout.write(line)

