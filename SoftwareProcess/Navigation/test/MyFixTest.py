import unittest
import uuid
import Navigation.prod.Fix as Fix
import os
import re
class FixTest(unittest.TestCase):
    
    def setUp(self):
        self.className = "Fix."
        # set default log file name
        self.DEFAULT_LOG_FILE = "log.txt"
        if(os.path.isfile(self.DEFAULT_LOG_FILE)):
            os.remove(self.DEFAULT_LOG_FILE)      
        # generate random log file name
        self.RANDOM_LOG_FILE = "log" + str(uuid.uuid4())[-12:] + ".txt"
    
#    Acceptance Test: 100
#        Analysis - Contructor
#            inputs
#                name of log file express as string which has a length .GE. 1
#            outputs
#                An instance of Fix
#            state change
#                Writes the following entry to the log file:
#                    The literal string "Log file:"                   followed by
#                    a tab (i.e., "\t")                               followed by
#                    the absolute file path of the log file            
#            Happy path
#                nominal case with string input: Fix("log.txt")
#                nominal case missing input: Fix()
#            Sad path
#                The file name violates the parameter specfication: Fix(1)
#                Input as a empty string: Fix("")
#    Happy path
    def test100_010_ShouldCreateInstanceOfFixUsingInput(self):
        self.assertIsInstance(Fix.Fix("log.txt"), Fix.Fix)
        
    def test100_020_ShouldCreateInstanceOfFixWithOutInput(self):
        theFix = Fix.Fix(self.RANDOM_LOG_FILE)
        logStartString = os.path.abspath(self.RANDOM_LOG_FILE)
        try:
            theLogFile = open(self.RANDOM_LOG_FILE, 'r')
            entry = theLogFile.readline()
            del theLogFile
            self.assertNotEquals(-1, entry.find(logStartString), 
                                 "Minor:  first line of log is incorrect")
        except IOError:
            self.fail()
        self.assertIsInstance(theFix, Fix.Fix, "major:  log file failed to create")
        self.cleanup()
    
#    Sad path
    def test100_910_ShouldRaiseIssueOnErrorInput(self):
        expectedDiag = self.className + "__init__:"
        with self.assertRaises(ValueError) as context:
            Fix.Fix(1)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test100_920_ShouldRaiseIssueOnEmptyStringInput(self):
        expectedDiag = self.className + "__init__:"
        with self.assertRaises(ValueError) as context:
            Fix.Fix("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
#    Acceptance Test: 200
#        Analysis - setSightingFile
#            inputs
#                name of XML file express as string which has a length .GE. 1
#            outputs
#                A string whose value is the absolute file path of the file specified by the parameter.              
#            state change
#                Writes the following entry to the log file:
#                   The literal, "Sighting file:"                    followed by
#                    a tab (i.e., "\t")                              followed by
#                    the absolute file path of the sighting file            
#            Happy path
#                nominal case with string input: setSightingFile("sf.xml")
#            Sad path
#                The file name is invalid      
#    Happy path
    def test200_010_ShouldReturnStringFilePath(self):
        anFix = Fix.Fix()
        sigthingFilePath = os.path.abspath("sf.xml")
        self.assertEqual(anFix.setSightingFile("sf.xml"), sigthingFilePath)
    def test200_020_ShouldWriteStringFilePathInLogFile(self):
        theFix = Fix.Fix(self.RANDOM_LOG_FILE)
        result = theFix.setSightingFile("sf.xml")
        logSightingString = os.path.abspath("sf.xml")
        try:
            theLogFile = open(self.RANDOM_LOG_FILE, 'r')
            entry = theLogFile.readlines()
            del theLogFile
            self.assertNotEquals(-1, entry[1].find(logSightingString), 
                                 "Minor:  line of log is incorrect")
        except IOError:
            self.fail()
        self.assertEqual(result, logSightingString)
        self.cleanup()
#    Sad path
    def test200_910_ShouldRaiseIssueOnInvalidFileName(self):
        expectedDiag = self.className + "setSightingFile"
        anFix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            anFix.setSightingFile()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 

#    Acceptance Test: 300
#        Analysis - setAriesFile
#            inputs
#                name of Aries file express as string which has a length .GE. 1 and ".txt" is literal file extension
#            outputs
#                A string whose value is the absolute file path of the file specified by the parameter.              
#            state change
#                Writes the following entry to the log file:
#                    The literal, "Aries file:"             followed by 
#                            a tab (i.e., "\t")             followed by 
#                            the absolute file path of the Aries file
#            Happy path
#                nominal case with string input: setAriesFile("aries.txt")
#            Sad path
#                The file name is invalid      
#    Happy path
    def test300_010_ShouldReturnStringFilePath(self):
        aFix = Fix.Fix()
        ariesFilePath = os.path.abspath("aries.txt")
        self.assertEqual(aFix.setAriesFile("aries.txt"), ariesFilePath)
        
    def test300_020_ShouldWriteStringFilePathInLogFile(self):
        theFix = Fix.Fix(self.RANDOM_LOG_FILE)
        theFix.setSightingFile("sf.xml")
        result = theFix.setAriesFile("aries.txt")
        logString = os.path.abspath("aries.txt")
        try:
            theLogFile = open(self.RANDOM_LOG_FILE, 'r')
            entry = theLogFile.readlines()
            del theLogFile
            self.assertNotEquals(-1, entry[2].find(logString), 
                                 "Minor:  line of log is incorrect")
        except IOError:
            self.fail()
        self.assertEqual(result, logString)
        self.cleanup()
#    Sad path
    def test300_910_ShouldRaiseIssueOnInvalidFileName(self):
        expectedDiag = self.className + "setAriesFile"
        aFix =Fix.Fix()
        with self.assertRaises(ValueError) as context:
            aFix.setAriesFile()
        self.assertEqual(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
#    Acceptance Test: 400
#        Analysis - setStarFile
#            inputs
#                name of Aries file express as string which has a length .GE. 1 and ".txt" is literal file extension
#            outputs
#                A string whose value is the absolute file path of the file specified by the parameter.              
#            state change
#                Writes the following entry to the log file:
#                    The literal, "Star file:"              followed by 
#                            a tab (i.e., "\t")             followed by 
#                            the absolute file path of the Star file
#            Happy path
#                nominal case with string input: setStarFile("stars.txt")
#            Sad path
#                The file name is invalid      
#    Happy path
    def test400_010_ShouldReturnStringFilePath(self):
        aFix = Fix.Fix()
        starsFilePath = os.path.abspath("stars.txt")
        self.assertEqual(aFix.setStarFile("stars.txt"), starsFilePath)
    
    def test400_020_ShouldWriteStringFilePathInLogFile(self):
        theFix = Fix.Fix(self.RANDOM_LOG_FILE)
        theFix.setSightingFile("sf.xml")
        theFix.setAriesFile("aries.txt")
        result = theFix.setStarFile("stars.txt")
        logString = os.path.abspath("stars.txt")
        try:
            theLogFile = open(self.RANDOM_LOG_FILE, 'r')
            entry = theLogFile.readlines()
            del theLogFile
            self.assertNotEquals(-1, entry[3].find(logString), 
                                 "Minor:  line of log is incorrect")
        except IOError:
            self.fail()
        self.assertEqual(result, logString)
        self.cleanup()
#    Sad path
    def test400_910_ShouldRaiseIssueOnInvalidFileName(self):
        expectedDiag = self.className + "setStarFile"
        aFix =Fix.Fix()
        with self.assertRaises(ValueError) as context:
            aFix.setStarFile()
        self.assertEqual(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
#    Acceptance Test: 500
#        Analysis - getSightings
#            inputs
#                none
#            outputs
#                Returns a tuple consisting of the latitude and longitude of the approximate location.
#            state change
#                Navigational calculations are written to the log file           
#            Happy path
#                nominal case
#            Sad path
#                No sighting file has been set
#                Errors are encountered in the sighting file
#                The observed altitude is .LT. 0.1 arc-minutes
#   Happy path
    def test500_010_ShouldReturnLaAndLoOfLocation(self):
        anFix = Fix.Fix()
        assertResult = "0d0.0", "0d0.0"
        anFix.setSightingFile("sf.xml")
        anFix.setAriesFile("aries.txt")
        anFix.setStarFile("stars.txt")
        self.assertEqual(assertResult, anFix.getSightings())
    
    def test500_020_ShouldWriteStringFilePathInLogFile(self):
        assertResult = "0d0.0", "0d0.0"
        theFix = Fix.Fix(self.RANDOM_LOG_FILE)
        theFix.setSightingFile("CA03TestSightingFile.xml")
        theFix.setAriesFile("aries.txt")
        theFix.setStarFile("stars.txt")
        result = theFix.getSightings()
        logString = "Pollux\t2017-04-14\t23:50:14\t15d1.5\t27d59.1\t84d33.4"
        try:
            theLogFile = open(self.RANDOM_LOG_FILE, 'r')
            entry = theLogFile.readlines()
            del theLogFile
            self.assertNotEquals(-1, entry[5].find(logString), 
                                 "Minor:  line of log is incorrect")
        except IOError:
            self.fail()
        self.assertEqual(assertResult, result)
        self.cleanup()
    
    def test500_030_ShouldCountSightingErrors(self):
        assertResult = "0d0.0", "0d0.0"
        theFix = Fix.Fix(self.RANDOM_LOG_FILE)
        theFix.setSightingFile("CA03TestSightingFile.xml")
        theFix.setAriesFile("aries.txt")
        theFix.setStarFile("stars.txt")
        result = theFix.getSightings()
        sightingErrors = "1"
        errorKey = "Sighting errors:"
        try:
            theLogFile = open(self.RANDOM_LOG_FILE, 'r')
            entry = theLogFile.readlines()
            del theLogFile
            for oneLine in entry:
                if oneLine.find(errorKey) != -1:
                    stringSet = re.split("\t|\n", oneLine)
                    for i in range(len(stringSet)):
                        if stringSet[i] == errorKey:
                            self.assertEqual(stringSet[i + 1], sightingErrors, "Singhing errors numbers is wrong")
                            break
                    break
        except IOError:
            self.fail()
        self.assertEqual(assertResult, result)
        self.cleanup()
#   Sad path
    def test500_910_ShouldRaiseIssueOnNoSightingFileSet(self):
        expectedDiag = self.className + "getSightings"
        anFix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            anFix.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
 
#  helper methods
    def indexInList(self, target, searchList):
        for index in range(len(searchList)):
            if(target in searchList[index]):
                return index
        return -1
    
    def cleanup(self):
        if(os.path.isfile(self.RANDOM_LOG_FILE)):
            os.remove(self.RANDOM_LOG_FILE)  