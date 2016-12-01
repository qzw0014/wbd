from Navigation.prod.Fix import Fix
myFix = Fix()
myFix.setSightingFile("CA05sightings2.xml")
myFix.setAriesFile("aries.txt")
myFix.setStarFile("stars.txt")
assumedLatitude = "N0d0.1"
assumedLongitude = "18d32"
try:
    myFix.getSightings(assumedLatitude, assumedLongitude)
except ValueError as e:
    print e