
states<- subset(states, !(states@data$STATE %in% c("Alaska", "Hawaii", "Puerto Rico",
	"U.S. Virgin Islands")))
states<- subset(states,!(AREA %in% c(1.166,0.994,0.790,11.527,2.739,0.443,0.065,0.211)))
lps<- coordinates(states)
ID<- rep(1, nrow(lps))
# DISSOLVE STATES FOR OUTLINE OF U.S.
states_dissolve <- unionSpatialPolygons(states,ID)	
	
	
	
missouri<- subset(hucs, hucs@data$HUC2==10)
lps<- coordinates(missouri)
ID<- rep(1, nrow(lps))
missouri <- unionSpatialPolygons(missouri,ID)