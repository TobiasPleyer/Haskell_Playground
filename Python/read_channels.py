import sys
import numpy as np
import matplotlib.pyplot as plt

BIT_TIME = 8.66e-06
MESSAGE_FINISHED_THRESHOLD = 2e-04

with open(sys.argv[1], 'r') as fp:
    ts = []
    cs1 = []
    cs2 = []
    fp.readline()
    for line in fp:
        t, c1, c2 = eval(line)
        ts.append(t)
        cs1.append(c1)
        cs2.append(c2)

ts = np.array(ts)
time_diffs = ts[1:] - ts[:-1]
indices = np.arange(time_diffs.size) + 1
border_indices = indices[time_diffs > MESSAGE_FINISHED_THRESHOLD]
chunk_starts = border_indices[:-1] + 1
chunk_starts[0] -= 1
chunk_ends = border_indices[1:]
chunks = zip(chunk_starts.tolist(), chunk_ends.tolist())
cs1 = np.array(cs1)
cs2 = np.array(cs2)
channel1 = {}
channel2 = {}

chunks = list(chunks)
print(chunks[:10])
for start, end in chunks[:10]:
    timestamp = ts[start]
    data1 = cs1[start:end]
    data2 = cs2[start:end]
    if data1.std() > data2.std():
        channel = channel1
    else:
        channel = channel2
    print("timestamp: {}".format(timestamp))
    print("time diff max: {}".format(time_diffs[start:end-1].max()))
    print((time_diffs[start:end-1] / BIT_TIME).round().sum())
