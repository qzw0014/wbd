from Navigation.prod.Fix import Fix
myFix = Fix()
myFix.setSightingFile("CA03TestSightingFile.xml")
myFix.setAriesFile("aries.txt")
myFix.setStarFile("stars.txt")
myFix.getSightings()
