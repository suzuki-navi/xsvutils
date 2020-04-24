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
        if re.compile(r"\A([-+])?[0-9]+(\.[0-9]*)?\Z").match(x):
            x = float(x)
            xs.append(x)
            records.append(cols)

x = []
y = []
c = []
for i in range(len(header)):
    h = header[i]
    for j in range(len(records)):
        ye = records[j][i]
        if re.compile(r"\A([-+])?[0-9]+(\.[0-9]*)?\Z").match(ye):
            x.append(xs[j])
            y.append(float(ye))
            c.append(i)
plt.scatter(x, y, c = c)

plt.savefig(chart_path, format=image_fmt)

#print(header)
#print(records)
#sys.stdout.write(line)

