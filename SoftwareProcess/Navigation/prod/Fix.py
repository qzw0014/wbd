import re
import time
import datetime
import os
import xml.etree.ElementTree as ET 
from Navigation.prod.Sighting import Sighting
from math import sqrt, radians, sin, cos, degrees
from math import tan
from Navigation.prod.Angle import Angle
from Navigation.sandbox.SightingSandBox import result
from numpy import arcsin, arccos
class Fix(object):
    
    
    def __init__(self, logFile = "log.txt"):
        self.logFileName = logFile
        self.xmlDataTree = None
        self.sigthingsList = []
        self.sightingFile = None
        self.ariesFile = None
        self.starsFile = None
        self.starNamesList = []
        self.sightingErrors = 0
        if isinstance(logFile, str):
            if logFile != "":
                try:
                    log = open(self.logFileName, "a")
                    logAbsolutePath = os.path.abspath(logFile)
                    log.write("LOG:\t" + self.getTime() + ":\tLog file:\t" + logAbsolutePath + "\n")
                    log.close()
                except:
                    raise ValueError("Fix.__init__:  The file can not be created or appended.")
            else:
                raise ValueError("Fix.__init__:  The file name violates the parameter specfication.")
        else:
            raise ValueError("Fix.__init__:  The file name violates the parameter specfication.")
    
    
    def setSightingFile(self, sightingFile = None):
        self.sightingFile = sightingFile
        try:
            self.xmlDataTree = ET.parse(sightingFile)
        except:
            raise ValueError("Fix.setSightingFile:  The file name is invalid.")
        sightingFileAbsPath = os.path.abspath(self.sightingFile)
        log = open(self.logFileName, "a")
        log.write("LOG:\t" + self.getTime() + ":\tSighting file: " + sightingFileAbsPath + "\n")
        log.close()
        return sightingFileAbsPath
    
    
    def setAriesFile(self, ariesFile = ""):
        pattern = re.compile(r'.+\.txt$')
        try:
            result = pattern.match(ariesFile)
        except:
            raise ValueError("Fix.setAriesFile:  The file name is invalid.")
        if result:
            try:
                openFile = open(ariesFile, "r")
                self.ariesFile = openFile.readlines()
                openFile.close()
            except:
                raise ValueError("Fix.setAriesFile:  The file name is invalid.")
            ariesAbsPath = os.path.abspath(ariesFile)
            log = open(self.logFileName, "a")
            log.write("LOG:\t" + self.getTime() + ":\tAries file:\t" + ariesAbsPath + "\n")
            log.close()
            return ariesAbsPath
        else:
            raise ValueError("Fix.setAriesFile:  The file name is invalid.")

    
    def setStarFile(self, starFile = ""):
        pattern = re.compile(r'.+\.txt$')
        try:
            result = pattern.match(starFile)
        except:
            raise ValueError("Fix.setStarFile:  The file name is invalid.")
        if result:
            try:
                openFile = open(starFile, "r")
                self.starsFile = openFile.readlines()
                openFile.close()
                self.getStarNames()
            except:
                raise ValueError("Fix.setStarFile:  The file name is invalid.")
            starsAbsPath = os.path.abspath(starFile)
            log = open(self.logFileName, "a")
            log.write("LOG:\t" + self.getTime() + ":\tStar file:\t" + starsAbsPath + "\n")
            log.close()
            return starsAbsPath
        else:
            raise ValueError("Fix.setStarFile:  The file name is invalid.")
    
    
    def getSightings(self, assumedLatitude = "0d0.0", assumedLongitude = "0d0.0"):
        if not self.getSightingInputCheak(assumedLatitude, assumedLongitude):
            raise ValueError("Fix.getSightings:  The parameters violate the specification.")
        if not self.fileSetCheck():
            raise ValueError("Fix.getSightings:  some file has been set.")
        else:
            xmlDataRoot = self.xmlDataTree.getroot()
            for sighting in xmlDataRoot.findall("sighting"):
                if self.xmlDataCheck(sighting):
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
                    result = self.getLongitudeAndLatitude(aSighting)
                    if result != False:
                        aSighting.set_latitude(result[0])
                        aSighting.set_longitude(result[1])
                        aSighting.set_assumedLatitude(assumedLatitude)
                        aSighting.set_assumedLongitude(assumedLongitude)
                        distanceAndAzimuthAdjustment = self.calculateDistanceAndAzimuthAdjustment(aSighting)
                        aSighting.set_distanceAdjustment(distanceAndAzimuthAdjustment[0])
                        aSighting.set_azimuthAdjustment(distanceAndAzimuthAdjustment[1])
                        self.sigthingsList.append(aSighting)
                    else:
                        self.sightingErrors += 1
                else:
                    self.sightingErrors += 1
            self.sigthingsList.sort(key=lambda Sighting:(Sighting.index, Sighting.body))
            approLatAndLon = self.calculateApproximateLatAndLon()
            approximateLatitide = approLatAndLon[0]
            approximateLongitide = approLatAndLon[1]
            log = open(self.logFileName, "a")
            for sighting in self.sigthingsList:
                log.write("LOG:\t" + self.getTime() + ":\t" + sighting.get_body() + "\t" + sighting.get_date() + "\t" + sighting.get_time()  + "\t" + sighting.get_adjustedAltitude() + "\t" + sighting.get_latitude() 
                          + "\t" + sighting.get_longitude() + "\t" + sighting.get_assumedLatitude() + "\t" + sighting.get_assumedlongitude() + "\t" + sighting.get_azimuthAdjustment() + "\t" + sighting.get_distanceAdjustment() + "\n")
            log.write("LOG:\t" + self.getTime() + ":\tSighting errors:" + "\t" + str(self.sightingErrors) + "\n")
            log.close()
            
            return (approximateLatitide, approximateLongitide)
    
    
    def calculateAdjustedAltitude(self, sighting):
        horizon = sighting.get_horizon()
        altitude = sighting.get_observation()
        pressure = int(sighting.get_pressure())
        temperature = float(sighting.get_temperature())
        height = float(sighting.get_height())
        arcAngle = Angle()
        arcAngle.setDegrees(altitude)
        if horizon == "natural":
            dip = (-0.97 * sqrt(height)) / 60
        elif  horizon == "artificial": 
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
    
    
    def fileSetCheck(self):
        if self.ariesFile == None or self.starsFile == None or self.xmlDataTree == None:
            return False
        else:
            return True
    
    
    def xmlDataCheck(self, sighting):
        try:
            body = sighting.find("body").text
            date = sighting.find("date").text
            time = sighting.find("time").text
            observation = sighting.find("observation").text
        except:
            return False
        try:
            height = sighting.find("height").text
        except:
            height = "0"
        try:  
            temperature = sighting.find("temperature").text
        except:
            temperature = "72"
        try:
            pressure = sighting.find("pressure").text
        except:
            pressure = "1010"
        try:
            horizon = sighting.find("horizon").text
        except:
            horizon = "natural"
        # Data Format Check
        if not self.bodyFormatCheck(body):
            return False
        if not self.dateFormatCheck(date):
            return False
        if not self.timeFormatCheck(time):
            return False
        if not self.observationFormatCheck(observation):
            return False
        if not self.heightFormatCheck(height):
            return False
        if not self.temperatureFormatCheck(temperature):
            return False
        if not self.pressureFormatCheck(pressure):
            return False
        if not self.horizonFormatCheck(horizon):
            return False
        return True
        
    
    def bodyFormatCheck(self,bodyValue):
        if bodyValue == None or bodyValue == "":
            return False
        elif bodyValue not in self.starNamesList:
            return False
        else:
            return True

      
    def dateFormatCheck(self, dateValue):
        try:
            time.strptime(dateValue, "%Y-%m-%d")
            return True
        except:
            return False
    
    
    def timeFormatCheck(self, timeValue):
        try:
            time.strptime(timeValue, "%H:%M:%S")
            return True
        except:
            return False
        
        
    def observationFormatCheck(self, observationValue):
        anAngle = Angle()
        minimumAngle = Angle()
        minimumAngle.setDegreesAndMinutes("0d0.1")
        try:
            anAngle.setDegreesAndMinutes(observationValue)
        except:
            return False
        if self.observationMinutesCheck(observationValue):
            if anAngle.getDegrees() > minimumAngle.getDegrees() and anAngle.getDegrees() < 90:
                return True
            else:
                return False
        else:
            return False
        
    
    def observationMinutesCheck(self, observationValue):
        pattern = re.compile(r'-?\d+\.?\d?')
        try:
            result = pattern.findall(observationValue)
        except:
            return False
        if float(result[1]) >= 0 and float(result[1]) < 60:
            return True
        else:
            return False
        
    
    def heightFormatCheck(self, heightValue):
        try:
            height = float(heightValue)
        except:
            return False
        if height < 0:
            return False
        else:
            return True
        
        
    def temperatureFormatCheck(self, temperatureValue):
        try:
            temperature = int(temperatureValue)
        except:
            return False
        if temperature >= -20 and temperature <= 120:
            return True
        else:
            return False
    
    
    def pressureFormatCheck(self, pressureValue):
        try:
            pressure = int(pressureValue)
        except:
            return False
        if pressure >=100 and pressure <= 1100:
            return True
        else:
            return False
        
        
    def horizonFormatCheck(self, horizonValue):
        horizon = horizonValue.lower()
        if horizon == "natural" or horizon == "artificial":
            return True
        else:
            return False
        
        
    def getLongitudeAndLatitude(self, sighting):
        body = sighting.get_body()
        date = self.dateFormatSwitch(sighting.get_date())
        time = sighting.get_time()
        longtitude = Angle()
        SHA = Angle()
        GHA = self.getGHA(date, time)
        starDataSet = self.getLatitudeAndSHA(body, date)
        if starDataSet == False or GHA == False:
            return False
        else:
            SHA.setDegreesAndMinutes(starDataSet[2])
            latitude = starDataSet[3]
            longtitude.add(GHA)
            longtitude.add(SHA)
            return(latitude, longtitude.getString())
        
        
    def getLatitudeAndSHA(self, body, date):
        spliteKey = re.compile(r'\t|\n')        
        starsDataSetList = []
        starDataSet = [] 
        for oneLine in self.starsFile:
            if oneLine.find(body) == 0:
                dataSet = spliteKey.split(oneLine)
                if len(dataSet) == 5:
                    starsDataSetList.append(dataSet)
        starsDataSetList.sort(key=lambda starsDataSetList: starsDataSetList[1])
        for i in range(len(starsDataSetList)):
            if starsDataSetList[i][1] > date:
                starDataSet = starsDataSetList[i-1]
                break
            if i == (len(starsDataSetList) - 1):
                starDataSet = starsDataSetList[i]
        if self.starDataCheck(starDataSet[2], starDataSet[3]):
            return starDataSet
        else:
            return False
    
    
    def getGHA(self, date, time):
        GHA1 = Angle()
        GHA2 = Angle()
        GHA = Angle()
        ariesDataSet = []
        timeSplitKey = re.compile(r':')
        timeSet = timeSplitKey.split(time)
        if timeSet[0] == "23":
            nextTime = "0"
            nextDate = self.dateCalculate(date, "1")
        else:
            nextTime = str(int(timeSet[0]) + 1)
            nextDate = date
        ariesDataSplitKey = re.compile(r'\t|\n')
        s = float(timeSet[1]) * 60 + float(timeSet[2])
        keyOne = date + "\t" + str(int(timeSet[0]))
        keyTwo = nextDate + "\t" + nextTime
        for oneLine in self.ariesFile:
            if oneLine.find(keyOne) == 0:
                ariesDataSet.append(ariesDataSplitKey.split(oneLine))
            if oneLine.find(keyTwo) == 0:
                ariesDataSet.append(ariesDataSplitKey.split(oneLine))            
        if len(ariesDataSet) >= 2:
            try:
                GHA1.setDegreesAndMinutes(ariesDataSet[0][2])
                GHA2.setDegreesAndMinutes(ariesDataSet[1][2])
            except:
                return False
            calculation = abs(GHA2.getDegrees() - GHA1.getDegrees()) * (s / 3600) + GHA1.getDegrees()
            GHA.setDegrees(calculation)
            return GHA
        else:
            return False
    
    
    def starDataCheck(self, SHA, latitude):
        SHAAngle = Angle()
        latitudeAngle = Angle()
        try:
            SHAAngle.setDegreesAndMinutes(SHA)
            latitudeAngle.setDegreesAndMinutes(latitude)
        except:
            return False
        return True
    
    
    def getStarNames(self):
        splitKey = re.compile(r'\t|\n')
        for oneLine in self.starsFile:
            dataSet = splitKey.split(oneLine)
            if len(dataSet) == 5 and dataSet[0] not in self.starNamesList:
                self.starNamesList.append(dataSet[0]) 
                
    
    def dateFormatSwitch(self, dateValue):
        temp = time.strptime(dateValue, "%Y-%m-%d")
        date = time.strftime("%m/%d/%y", temp)
        return date
    
    
    def dateCalculate(self, date, delta):
        d1 = datetime.datetime.strptime(date, "%m/%d/%y")
        days = datetime.timedelta(days = int(delta))
        d2 = d1 + days
        return d2.strftime("%m/%d/%y")
    
    
    def getSightingInputCheak(self, assumedLatitude, assumedLongitude):
        if self.assumedLatitudeCheck(assumedLatitude) and self.assumedLongitudeCheck(assumedLongitude):
            return True
        else:
            return False
        
    
    def assumedLatitudeCheck(self, assumedLatitude):
        splitKey = re.compile(r'(\d+d\d+\.?\d?$)')
        try:
            resultList = splitKey.split(assumedLatitude)
        except:
            return False
        if resultList[0] == "N" or resultList[0] == "S":
            if self.observationFormatCheck(resultList[1]):
                if self.observationMinutesCheck(resultList[1]):
                    return True
                else:
                    return False
            elif resultList[0] == "" and resultList[1] == "0d0.0":
                return True
            else:
                return False
        
        
    def assumedLongitudeCheck(self, assumedLongitude):
        anAngle = Angle()
        try:
            anAngle.setDegreesAndMinutes(assumedLongitude)
        except:
            return False
        pattern = re.compile(r'\d+\.?\d?')
        try:
            result = pattern.findall(assumedLongitude)
        except:
            return False
        if float(result[1]) >= 0 and float(result[1]) < 60:
            if float(result[0]) >= 0 and float(result[0]) < 360:
                return True
            else:
                return False
        else:
            return False
             
         
    def calculateApproximateLatAndLon(self):
        if len(self.sigthingsList) == 0:
            return
        else:
            assLatAngle = Angle()
            assLonAngle = Angle()
            assLatAngle.setDegreesAndMinutes(self.sigthingsList[0].get_assumedLatitudeWithoutOrientation())
            assLonAngle.setDegreesAndMinutes(self.sigthingsList[0].get_assumedlongitude())
            assumedLatitude = assLatAngle.getDegrees()
            assumedLongitude = assLonAngle.getDegrees()
            sumOfLat = 0.0
            sumOfLon = 0.0
            for oneSighting in self.sigthingsList:
                azimuthAngle = Angle()
                distanceAdjustment = float(oneSighting.get_distanceAdjustment())
                azimuthAngle.setDegreesAndMinutes(oneSighting.get_azimuthAdjustment())
                sumOfLat = sumOfLat + distanceAdjustment + cos(radians(azimuthAngle.getDegrees()))
                sumOfLon = sumOfLon + distanceAdjustment + sin(radians(azimuthAngle.getDegrees()))
            approximateLatitude = assumedLatitude + sumOfLat / 60
            approximateLongitude = assumedLongitude + sumOfLon / 60
            return(approximateLatitude, approximateLongitude)
    
    
    def calculateDistanceAndAzimuthAdjustment(self, sighting):
        aziAngle = Angle()
        assumedLongitude = sighting.get_assumedlongitude()
        assumedLatitude = sighting.get_assumedLatitudeWithoutOrientation()
        geoPosLongitude = sighting.get_longitude()
        geoPosLatitude = sighting.get_latitude()
        adjustedAltitude = sighting.get_adjustedAltitude()
        LHA = self.getLHA(assumedLongitude, geoPosLongitude)
        corAltAndInterDis = self.getCorrectedAltitude(geoPosLatitude, assumedLatitude, LHA)
        correctedAltitude = corAltAndInterDis[0]
        intermediateDistance = corAltAndInterDis[1]
        distanceAdjustment = self.getDistanceAdjustment(adjustedAltitude, correctedAltitude)
        azimuthAdjustment = self.getAzimuthAdjustment(geoPosLatitude, assumedLatitude, correctedAltitude, intermediateDistance)
        aziAngle.setDegrees(azimuthAdjustment)
        return(str(distanceAdjustment), aziAngle.getString())
    
    
    def getLHA(self, assumedLongitude, geoPosLongitude):
        assLonAngle = Angle()
        geoPosLonAngle = Angle()
        assLonAngle.setDegreesAndMinutes(assumedLongitude)
        geoPosLonAngle.setDegreesAndMinutes(geoPosLongitude)
        LHA = geoPosLonAngle.getDegrees() + assLonAngle.getDegrees()
        return LHA
    
    
    def getCorrectedAltitude(self, geoPosLatitude, assumedLatitude, LHA):
        geoPosLatAngle = Angle()
        assLatAngle = Angle()
        geoPosLatAngle.setDegreesAndMinutes(geoPosLatitude)
        assLatAngle.setDegreesAndMinutes(assumedLatitude)
        intermediateDistance = (sin(radians(geoPosLatAngle.getDegrees())) * sin(radians(assLatAngle.getDegrees()))) + (cos(radians(geoPosLatAngle.getDegrees())) * cos(radians(assLatAngle.getDegrees())) * cos(radians(LHA)))
        correctedAltitude = arcsin(intermediateDistance)
        correctedAltitude = degrees(correctedAltitude)
        return (correctedAltitude, intermediateDistance)
    
    
    def getDistanceAdjustment(self, adjustedAltitude, correctedAltitude):
        adjAltAngle = Angle()
        adjAltAngle.setDegreesAndMinutes(adjustedAltitude)
        distanceAdjustment = correctedAltitude - adjAltAngle.getDegrees()
        distanceAdjustment = int(round(distanceAdjustment * 60))
        return distanceAdjustment
    
    
    def getAzimuthAdjustment(self, geoPosLatitude, assumedLatitude, correctedAltitude, intermediateDistance):
        geoPosLatAngle = Angle()
        assLatAngle = Angle()
        geoPosLatAngle.setDegreesAndMinutes(geoPosLatitude)
        assLatAngle.setDegreesAndMinutes(assumedLatitude)
        mem = (sin(radians(geoPosLatAngle.getDegrees())) - sin(radians(assLatAngle.getDegrees())) * intermediateDistance)
        deno = (cos(radians(assLatAngle.getDegrees())) * cos(radians(correctedAltitude)))
        tem = (mem / deno)
        azimuthAdjustment = arccos(tem)
        azimuthAdjustment = degrees(azimuthAdjustment)
        return azimuthAdjustment