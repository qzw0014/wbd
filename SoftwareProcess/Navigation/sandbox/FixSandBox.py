from Navigation.prod.Fix import Fix
myFix = Fix()
myFix.setSightingFile("CA05sightings2.xml")
myFix.setAriesFile("aries.txt")
myFix.setStarFile("stars.txt")
assumedLatitude = "N27d59.5"
assumedLongitude = "85d33.4"
print myFix.getSightings(assumedLatitude, assumedLongitude)
