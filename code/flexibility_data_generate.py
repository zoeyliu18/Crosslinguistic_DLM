import sys, io, csv, math, random


predictability = []
base1_predictability = []
base2_predictability = []

with io.open(sys.argv[1] + '-' + sys.argv[2] + '-base1-ppl.txt', encoding = 'utf-8') as f:
	for line in f:
		base1_predictability.append(float(line.split()[0]))

with io.open(sys.argv[1] + '-' + sys.argv[2] + '-base2-ppl.txt', encoding = 'utf-8') as f:
	for line in f:
		base2_predictability.append(float(line.split()[0]))

for i in range(len(base1_predictability)):
	predictability.append(base2_predictability[i] / base1_predictability[i])

len_diff = []
pp1_len = []
pp2_len = []

with io.open(sys.argv[1] + '-' + sys.argv[2] + '-pp1.txt', encoding = 'utf-8') as f:
	for line in f:
		pp1_len.append(len(line.split()))

with io.open(sys.argv[1] + '-' + sys.argv[2] + '-pp2.txt', encoding = 'utf-8') as f:
	for line in f:
		pp2_len.append(len(line.split()))

for i in range(len(pp1_len)):
	len_diff.append(pp2_len[i] - pp1_len[i])

pp1_distance = []
pp2_distance = []
distance_diff = []

with io.open(sys.argv[1] + '-' + sys.argv[2] + '-distance1.txt', encoding = 'utf-8') as f:
	for line in f:
		pp1_distance.append(int(line.split()[0]))

with io.open(sys.argv[1] + '-' + sys.argv[2] + '-distance2.txt', encoding = 'utf-8') as f:
	for line in f:
		pp2_distance.append(int(line.split()[0]))

for i in range(len(pp1_distance)):
	distance_diff.append(pp2_distance[i] - pp1_distance[i])

split = int(len(pp1_len) / 2)

features = []
for i in range(len(pp1_len)):
	features.append([predictability[i], base1_predictability[i], base2_predictability[i], len_diff[i], pp1_len[i], pp2_len[i], pp1_distance[i], pp2_distance[i], distance_diff[i]])

random.shuffle(features)

data = []
for i in range(split):
	data.append(['1', features[i][0], features[i][1], features[i][2], features[i][3], features[i][4], features[i][5], features[i][6], features[i][7], features[i][8]])

for i in range(split, len(pp1_len)):
	data.append(['0', 1 / features[i][0], features[i][2], features[i][1], -1 * features[i][3], features[i][5], features[i][4], features[i][7], features[i][7], -1 * features[i][8]])


header = ['Order', 'Predictability', 'PP1_predictability', 'PP2_predictability', 'Len_diff', 'PP1_len', 'PP2_len', 'PP1_distance', 'PP2_distance', 'Distance_diff']
with io.open(sys.argv[1] + '-' + sys.argv[2] + '-flexibility.csv', 'w', encoding = 'utf-8') as f:
	writer = csv.writer(f)
	writer.writerow(header)
	for tok in data:
		writer.writerow(tok)
