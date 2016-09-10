from Navigation.prod.Angle import Angle

myAngle1 = Angle()
myAngle2 = Angle()
myAngle3 = Angle()
'''
inputValue1 = [125, 125.5, 780, -100, -130.5, -960, "k"]

for value in inputValue1:
    try:
        myAngle1.setDegrees(value)
        print(myAngle1.getDegrees())
        print(myAngle1.getString())
    except ValueError as e:
        print(e)
    

'''
'''
inputValue2 = ["35d18", "380d62", "-180d30", "-960d30", "kki", "360", "0.1d0"]

for stringValue in inputValue2:
    try:
        myAngle2.setDegreesAndMinutes(stringValue)
        print(myAngle2.getString())
        print(myAngle2.getDegrees())
    except ValueError as e:
        print(e)
'''

myAngle1.setDegrees(-220.5)
myAngle2.setDegrees(150.5)

myAngle1.add(myAngle2)
print(myAngle1.getDegrees())
print(myAngle1.getString())

myAngle1.subtract(myAngle2)
print(myAngle1.getDegrees())
print(myAngle1.getString())