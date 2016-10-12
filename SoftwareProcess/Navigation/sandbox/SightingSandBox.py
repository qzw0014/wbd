import re
a = "12:55:30"
b = "20151-2"
pattern = re.compile(r'\d{2}')
result = pattern.findall(a)
for k in result:
    print k