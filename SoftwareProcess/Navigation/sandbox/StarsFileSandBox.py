import re


starsFile = open("stars.txt")
starContent = starsFile.readlines()
starsFile.close()
spliteKey = re.compile(r'\t|\n')
body = "Diphda"
date = "12/31/17"
starsDataSetList = []
satrDataSet = []
for oneLine in starContent:
    if oneLine.find(body) == 0:
        dataSet = spliteKey.split(oneLine)
        starsDataSetList.append(dataSet)
starsDataSetList.sort(key=lambda starsDataSetList: starsDataSetList[1])
for i in range(len(starsDataSetList)):
    if starsDataSetList[i][1] > date:
        satrDataSet = starsDataSetList[i-1]
        break
    if i == (len(starsDataSetList) - 1):
        satrDataSet = starsDataSetList[i]
print satrDataSet