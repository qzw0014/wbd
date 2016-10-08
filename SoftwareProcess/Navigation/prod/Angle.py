from tools import isNumber
from tools import islegal
from tools import spliter

class Angle():
    def __init__(self):
        self.degrees = 0
        self.minutes = 0
        pass
    
    def setDegrees(self, value = 0):
        if isNumber(value):
            self.degrees = value % 360
            self.minutes = round(((self.degrees - int(self.degrees)) * 60), 1)
            return float(self.degrees)
        else:
            raise ValueError("Angle.setDegrees:  The input of degrees is not numeric value")
    
    def setDegreesAndMinutes(self, degrees):
        if islegal(degrees):
            result = spliter(degrees)
            minTemp = round(float(result[1]) % 60,1)
            carry = int(float(result[1]) /60)
            if int(result[0]) < 0:
                self.degrees = ((int(result[0]) - carry) - (minTemp / 60)) % 360
                self.minutes = round(((self.degrees - int(self.degrees)) * 60), 1)
            else:
                self.degrees = ((int(result[0]) + carry) + (minTemp / 60)) % 360
                self.minutes = round(((self.degrees - int(self.degrees)) * 60), 1)
            return self.degrees
        else:
            raise ValueError("Angle.setDegreesAndMinutes:  The input of degrees is not numeric value")
        
    
    def add(self, angle = None):
        if isinstance(angle, Angle):
            leftAdder = self.getDegrees()
            rightAdder = angle.getDegrees()
            result = leftAdder + rightAdder
            self.setDegrees(result % 360)
            self.minutes = round(((self.degrees - int(self.degrees)) * 60), 1)
            return self.degrees
        else:
            raise ValueError("Angle.add:  The input value is not a valid instance of Angle")
    
    def subtract(self, angle = None):
        if isinstance(angle, Angle):
            leftSubtractor = self.getDegrees()
            rightSubtractor = angle.getDegrees()
            result = leftSubtractor - rightSubtractor
            self.setDegrees(result % 360)
            self.minutes = round(((self.degrees - int(self.degrees)) * 60), 1)
            return self.degrees
        else:
            raise ValueError("Angle.subtract:  The input value is not a valid instance of Angle")
    
    def compare(self, angle = None):
        if isinstance(angle, Angle):
            leftValue = self.getDegrees()
            rightValue = angle.getDegrees()
            if leftValue > rightValue:
                return 1
            elif leftValue == rightValue:
                return 0
            else:
                return  -1
        else:
            raise ValueError("Angle.compare:  The input value is not a valid instance of Angle")
        pass
    
    def getString(self):
        stringResult  = str(int(self.degrees)) + "d" + str(self.minutes)
        return stringResult
        pass
    
    def getDegrees(self):
        return (int(self.degrees) + (self.minutes / 60))