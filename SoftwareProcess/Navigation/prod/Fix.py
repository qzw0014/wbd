import re
import time
from datetime import datetime
import xml.etree.ElementTree as ET 
from Navigation.prod.Sighting import Sighting
from math import sqrt, radians
from math import tan
from Navigation.prod.Angle import Angle
class Fix(object):
    
    
    def __init__(self, logFile = "log.txt"):
        self.log = None
        self.xmlDataTree = None
        self.sigthingsList = []
        self.sightingFile = None
        if isinstance(logFile, str):
            if logFile != "":
                try:
                    self.log = open(logFile, "a")
                    self.log.write("LOG:\t" + self.getTime() + ":\tStart of log\n")
                except:
                    raise ValueError("Fix.Fix:  The file can not be created or appended.")
            else:
                raise ValueError("Fix.Fix:  The file name violates the parameter specfication.")
        else:
            raise ValueError("Fix.Fix:  The file name violates the parameter specfication.")
    
    
    def setSightingFile(self, sightingFile = None):
        self.sightingFile = sightingFile
        try:
            self.xmlDataTree = ET.parse(sightingFile)
        except:
            raise ValueError("Fix.setSightingFile:  The file name is invalid.")
        self.log.write("LOG:\t" + self.getTime() + ":\tStart of sighting file:" + sightingFile + "\n")
        return sightingFile
    
    
    def getSightings(self):
        if self.xmlDataTree == None:
            raise ValueError("Fix.getSightings:  No sighting file has been set.")
        else:
            xmlDataRoot = self.xmlDataTree.getroot()
            for sighting in xmlDataRoot.findall("sighting"):
                self.xmlDataCheck(sighting)
                aSighting = Sighting()
                aSighting.set_body(sighting.find("body").text)
                aSighting.set_date(sighting.find("date").text)
                aSighting.set_time(sighting.find("time").text)
                aSighting.set_observation(sighting.find("observation").text)
                aSighting.set_height(sighting.find("height").text)
                aSighting.set_temperature(sighting.find("temperature").text)
                aSighting.set_pressure(sighting.find("pressure").text)
                aSighting.set_horizon(sighting.find("horizon").text)
                aSighting.set_index()
                aSighting.set_adjustedAltitude(self.calculateAdjustedAltitude(aSighting))
                self.sigthingsList.append(aSighting)
            self.sigthingsList.sort(key=lambda Sighting:(Sighting.index, Sighting.body))
            for sighting in self.sigthingsList:
                self.log.write("LOG:\t" + self.getTime() + ":\t" + sighting.get_body() + "\t" + sighting.get_date() + "\t" + sighting.get_time()  + "\t" + sighting.get_adjustedAltitude() + "\n")
            self.log.write("LOG:\t" + self.getTime() + ":\tEnd of sighting file:" + self.sightingFile + "\n")
            self.log.close()
            approximateLatitide = "0d0.0"
            approximateLongitide = "0d0.0"
            return (approximateLatitide, approximateLongitide)
    
    
    def calculateAdjustedAltitude(self, sighting):
        horizon = sighting.get_horizon()
        altitude = sighting.get_observation()
        pressure = float(sighting.get_pressure())
        temperature = float(sighting.get_temperature())
        height = float(sighting.get_height())
        if horizon == "Natural":
            dip = (-0.97 * sqrt(height)) / 60
        else:
            dip = 0
        celsiusTemperature = (temperature - 32) / 1.8
        refraction = (-0.00452 * pressure) / (273 + celsiusTemperature) / tan(radians(altitude))           
        adjustedAltitude = altitude + dip + refraction
        resultAngle = Angle()
        resultAngle.setDegrees(adjustedAltitude)
        return resultAngle.getString()
    
    
    def getTime(self):
        myTime = time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time())) + ("-" if time.timezone > 0 else "+") + time.strftime('%H:%M', time.gmtime(abs(time.timezone)))
        return myTime
    
    
    def xmlDataCheck(self, sighting):
        errorMessage = "Fix.getSightings:  Errors are encountered in the sighting file."
        anAnlge = Angle()
        ninetyAngle = Angle()
        miniAngle = Angle()
        ninetyAngle.setDegrees(90)
        miniAngle.setDegreesAndMinutes("0d0.1")
        try:
            anAnlge.setDegreesAndMinutes(sighting.find("observation").text)
        except:
            raise ValueError(errorMessage)
        if sighting.find("body").text == None or sighting.find("body").text == "":
            raise ValueError(errorMessage)
        elif not self.dateFormatCheck(sighting.find("date").text):
            raise ValueError(errorMessage)
        elif not self.timeFormatCheck(sighting.find("time").text):
            raise ValueError(errorMessage)
        elif float(sighting.find("height").text) < 0:
            raise ValueError(errorMessage)
        elif float(sighting.find("temperature").text) < -20 or float(sighting.find("temperature").text) > 120:
            raise ValueError(errorMessage)
        elif float(sighting.find("pressure").text) < 100 or float(sighting.find("pressure").text) > 1100:
            raise ValueError(errorMessage)
        elif ninetyAngle.compare(anAnlge) != 1:
            raise ValueError(errorMessage)
        elif miniAngle.compare(anAnlge) != -1:
            raise ValueError(errorMessage)
    
    
    def dateFormatCheck(self, dateValue):
        try:
            datetime.strptime(dateValue, "%Y-%m-%d")
            return True
        except:
            return False
    
    
    def timeFormatCheck(self, timeValue):
        try:
            datetime.strptime(timeValue, "%H:%M:%S")
            return True
        except:
            return False
        
        
    def observationCheck(self, observationValue):
        pattern = re.compile(r'-?\d+\.?\d?')
        result = pattern.findall(observationValue)
        if float(result) < 60:
            return True
        else:
            return False
        
        