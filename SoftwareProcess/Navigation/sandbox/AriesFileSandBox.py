import re

ariesFile = open("aries.txt")
myContent = ariesFile.readlines()
ariesFile.close()
ariesDataSet = []
date = "08/03/17"
time = "20:38:40"
timeSplitKey = re.compile(r':')
timeSet = timeSplitKey.split(time)
if timeSet[0] == "23":
    nextTime = "0"
else:
    nextTime = str(int(timeSet[0]) + 1)
ariesDataSplitKey = re.compile(r'\t|\n')
s = timeSet[1] * 60 + timeSet[2]
for oneLine in myContent:
    if oneLine.find(date + "\t" + timeSet[0]) == 0:
        ariesDataSet.append(ariesDataSplitKey.split(oneLine))
    if oneLine.find(date + "\t" + nextTime) == 0:
        ariesDataSet.append(ariesDataSplitKey.split(oneLine))
print len(ariesDataSet)