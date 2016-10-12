from Navigation.prod.Fix import Fix
myFix = Fix()
myFix.setSightingFile("sfNeedSort.xml")
myFix.getSightings()
for sighting in myFix.sigthingsList:
    print sighting.get_index(), sighting.get_adjustedAltitude()