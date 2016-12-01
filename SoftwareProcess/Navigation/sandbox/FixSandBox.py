from Navigation.prod.Fix import Fix
myFix = Fix()
myFix.setSightingFile("CA05sightings2.xml")
myFix.setAriesFile("aries.txt")
myFix.setStarFile("stars.txt")
assumedLatitude = "27d59.5"
assumedLongitude = "85d33.4"
try:
    myFix.getSightings(assumedLatitude, assumedLongitude)
except:
    print