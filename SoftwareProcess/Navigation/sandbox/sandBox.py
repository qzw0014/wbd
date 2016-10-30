import datetime

d1 = datetime.datetime.strptime('12/31/91', '%m/%d/%y')
delta = datetime.timedelta(days=3)
d2 = d1 + delta
print d2.strftime("%m/%d/%y")