import unittest
import Navigation.prod.Fix as Fix
import os
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
        self.assertEqual(assertResult, anFix.getSightings())
#   Sad path
    def test500_910_ShouldRaiseIssueOnNoSightingFileSet(self):
        expectedDiag = self.className + "getSightings"
        anFix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            anFix.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_920_ShouldRaiseIssueOnEncounterWithoutBodyInSightingFile(self):
        expectedDiag = self.className + "getSightings"
        anFix = Fix.Fix()
        anFix.setSightingFile("ErrorSightingsWithoutBody.xml")
        with self.assertRaises(ValueError) as context:
            anFix.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_930_ShouldRaiseIssueOnEncounterIncorrectDateInSightingFile(self):
        expectedDiag = self.className + "getSightings"
        anFix = Fix.Fix()
        anFix.setSightingFile("ErrorSightingsIncorrectDate.xml")
        with self.assertRaises(ValueError) as context:
            anFix.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_940_ShouldRaiseIssueOnEncounterInvalidHeightInSightingFile(self):
        expectedDiag = self.className + "getSightings"
        anFix = Fix.Fix()
        anFix.setSightingFile("ErrorSightingInvalidHeight.xml")
        with self.assertRaises(ValueError) as context:
            anFix.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_950_ShouldRaiseIssueOnObservationLessThanMinimumArcMinutes(self):
        expectedDiag = self.className + "getSightings"
        anFix = Fix.Fix()
        anFix.setSightingFile("ErrorSightingsInvalidObservation.xml")
        with self.assertRaises(ValueError) as context:
            anFix.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        