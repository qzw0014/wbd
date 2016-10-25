import re
import time
import os
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
                    logAbsolutePath = os.path.abspath(logFile)
                    self.log.write("LOG:\t" + self.getTime() + ":\tLog file:\t" + logAbsolutePath + "\n")
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
        sightingFileAbsPath = os.path.abspath(self.sightingFile)
        self.log.write("LOG:\t" + self.getTime() + ":\tSighting file: " + sightingFileAbsPath + "\n")
        return sightingFile
    
    
    def getSightings(self):
        if self.xmlDataTree == None:
            raise ValueError("Fix.getSightings:  No sighting file has been set.")
        else:
            xmlDataRoot = self.xmlDataTree.getroot()
            for sighting in xmlDataRoot.findall("sighting"):
                self.xmlDataCheck(sighting)
                body = sighting.find("body").text
                date = sighting.find("date").text
                time = sighting.find("time").text
                observation = sighting.find("observation").text
                try:
                    height = sighting.find("height").text
                except:
                    height = None
                try:  
                    temperature = sighting.find("temperature").text
                except:
                    temperature = None
                try:
                    pressure = sighting.find("pressure").text
                except:
                    pressure = None
                try:
                    horizon = sighting.find("horizon").text
                except:
                    horizon = None
                aSighting = Sighting()
                aSighting.set_body(body)
                aSighting.set_date(date)
                aSighting.set_time(time)
                aSighting.set_observation(observation)
                aSighting.set_height(height)
                aSighting.set_temperature(temperature)
                aSighting.set_pressure(pressure)
                aSighting.set_horizon(horizon)
                aSighting.set_index()
                aSighting.set_adjustedAltitude(self.calculateAdjustedAltitude(aSighting))
                self.sigthingsList.append(aSighting)
            self.sigthingsList.sort(key=lambda Sighting:(Sighting.index, Sighting.body))
            for sighting in self.sigthingsList:
                self.log.write("LOG:\t" + self.getTime() + ":\t" + sighting.get_body() + "\t" + sighting.get_date() + "\t" + sighting.get_time()  + "\t" + sighting.get_adjustedAltitude() + "\n")
            self.log.write("LOG:\t" + self.getTime() + ":\tEnd of sighting file: " + self.sightingFile + "\n")
            self.log.close()
            approximateLatitide = "0d0.0"
            approximateLongitide = "0d0.0"
            return (approximateLatitide, approximateLongitide)
    
    
    def calculateAdjustedAltitude(self, sighting):
        errorMessage = "Fix.getSightings:  The observed altitude is .LT. 0.1 arc-minutes."
        horizon = sighting.get_horizon()
        altitude = sighting.get_observation()
        pressure = float(sighting.get_pressure())
        temperature = float(sighting.get_temperature())
        height = float(sighting.get_height())
        if height < 0:
            raise ValueError("Fix.getSightings:  Errors are encountered in the sighting file.")
        if temperature < -20 or temperature > 120:
            raise ValueError("Fix.getSightings:  Errors are encountered in the sighting file.")
        if pressure < 100 or pressure > 1100:
            raise ValueError("Fix.getSightings:  Errors are encountered in the sighting file.")
        arcAngle = Angle()
        arcAngle.setDegrees(altitude)
        miniArcAngle = Angle()
        miniArcAngle.setDegreesAndMinutes("0d0.1")
        if miniArcAngle.compare(arcAngle) == 1:
            raise ValueError(errorMessage)
        else:
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
        ninetyAngle.setDegrees(90)
        try:
            body = sighting.find("body").text
            date = sighting.find("date").text
            time = sighting.find("time").text
            observation = sighting.find("observation").text
        except:
            raise ValueError(errorMessage)
        try:
            anAnlge.setDegreesAndMinutes(observation)
        except:
            raise ValueError(errorMessage)
        if body == None or body == "":
            raise ValueError(errorMessage)
        elif not self.dateFormatCheck(date):
            raise ValueError(errorMessage)
        elif not self.timeFormatCheck(time):
            raise ValueError(errorMessage)
        elif not self.observationMinutesCheck(observation):
            raise ValueError(errorMessage)
        elif ninetyAngle.compare(anAnlge) != 1:
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
        
        
    def observationMinutesCheck(self, observationValue):
        pattern = re.compile(r'-?\d+\.?\d?')
        result = pattern.findall(observationValue)
        if float(result[1]) < 60:
            return True
        else:
            return False