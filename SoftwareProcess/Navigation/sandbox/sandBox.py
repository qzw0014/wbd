import re
latSplitKey = re.compile(r'(\d+d\d+\.?\d?$)')
resultList = latSplitKey.split("0d0.0")
print resultList
if resultList[0] == "":
    print "yes"