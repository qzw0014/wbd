import unittest
import Navigation.prod.Fix as Fix

class FixTest(unittest.TestCase):
    
    def setUp(self):
        self.className = "Fix."
    
    def tearDown(self):
        pass
    
#    Acceptance Test: 100
#        Analysis - Contructor
#            inputs
#                name of log file express as string which has a length .GE. 1
#            outputs
#                An instance of Fix
#            state change
#                Writes the following entry to the log file:
#                    Start of log
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
        self.assertIsInstance(Fix.Fix(), Fix.Fix)
    
#    Sad path
    def test100_910_ShouldRaiseIssueOnErrorInput(self):
        expectedDiag = self.className + "Fix:"
        with self.assertRaises(ValueError) as context:
            Fix.Fix(1)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test100_920_ShouldRaiseIssueOnEmptyStringInput(self):
        expectedDiag = self.className + "Fix:"
        with self.assertRaises(ValueError) as context:
            Fix.Fix("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
#    Acceptance Test: 200
#        Analysis - setSightingFile
#            inputs
#                name of XML file express as string which has a length .GE. 1
#            outputs
#                A string having the value passed as the "sightingFile".
#            state change
#                Writes the following entry to the log file:
#                    Start of sighting file f.xml
#                    where f.xml is the actual name of the file
#            Happy path
#                nominal case with string input: setSightingFile("sf.xml")
#            Sad path
#                The file name is invalid      
#    Happy path
    def test200_10_ShouldReturnString(self):
        anFix = Fix.Fix()
        self.assertIsInstance(anFix.setSightingFile("sf.xml"), str)
#    Sad path
    def test200_910_ShouldRaiseIssuseOnInvalidFileName(self):
        expectedDiag = self.className + "setSightingFile"
        anFix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            anFix.setSightingFile()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 

#    Acceptance Test: 300
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
    def test300_10_ShouldReturnLaAndLoOfLocation(self):
        