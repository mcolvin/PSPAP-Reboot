
# MISSOURI RIVER WATERSHED

## READ IN US HUCS
hucs<- readOGR("C:/Users/mcolvin/My Documents/projects/gis coverages", "hucs00p020")
## SUBSET OUT MO RIVER HUC	
missouri<- subset(hucs, hucs@data$HUC2==10)
## EXTRACT COORDINATES FOR EACH HUC CENTROID
lps<- coordinates(missouri)
## MAKE AN ID TO DISSOLVE HUCS WITH
ID<- rep(1, nrow(lps))
## MAKE A MO RIVER WATERSHED FOR PLOTING
missouri_ws <- unionSpatialPolygons(missouri,ID)




# LOAD AND CLEAN COVERAGE OF US STATES

## STATES
#states<- readOGR("C:/Users/mcolvin/My Documents/projects/gis coverages", "statep010")
## SUBSET CONTIGUOUS US STATES
#states<- subset(states, !(states@data$STATE %in% c("Alaska", "Hawaii", "Puerto Rico",
#	"U.S. Virgin Islands")))
## REMOVE SOME ISLANDS
#states<- subset(states,!(AREA %in% c(1.166,0.994,0.790,11.527,2.739,0.443,0.065,0.211)))
## EXTRACT COORDINES FOR EACH STATE CENTROID
#lps<- coordinates(states)
## MAKE AN ID TO DISSOLVE HUCS WITH
#ID<- rep(1, nrow(lps))
# DISSOLVE STATES FOR OUTLINE OF U.S.
#states_dissolve <- unionSpatialPolygons(states,ID)	
## COERCE TO SPATIALPOLYGONDATAFRAME
#ppp<-as(states_dissolve, "SpatialPolygonsDataFrame")
#writeOGR(ppp, 
#    "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
#    "states_dissolve", driver="ESRI Shapefile")

# LOAD PROCESSED US POLYGON
us<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
    "states_dissolve")	

# PALLID STURGEON MANAGEMENT UNITS

## LOAD DATA
manunits<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
    "PallidSturgeon_ManagementUnits")



    
# HATCHERY DATA
chan<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")
hatcheries<- sqlFetch(chan, "Hatchery_latlongs")

# LOAD MAJOR STREAMS - MAY BE REDUNDANCE WITH ABOVE, CONSOLIDATING CODE
majorstreams<- readOGR("C:/Users/mcolvin/My documents/projects/gis coverages/National Coverages", 
    "hydrogl020")


# WATER BODIES 

## LOAD DATA
#wb<- readOGR("C:/Users/mcolvin/My Documents/projects/gis coverages/National Coverages", 
#    "wtrbdyp010")
## SUBSET OUT MISSOURI RIVER RESERVOIRS
#reservoirs<- subset(wb, Name %in%c("Lake Sakakawea",
#    "Lake Francis Case","Lake Oahe", "Lake Sharpe", "Fort Peck Lake",
#    "Lewis and Clark Lake"))
## COERCE TO SPATIALPOLYGONDATAFRAME
#ppp<-as(reservoirs, "SpatialPolygonsDataFrame")
#writeOGR(ppp, 
#    "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
#    "reservoirs", driver="ESRI Shapefile")

## LOAD PROCESSED MAJOR WATERBODIES
reservoirs<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    "reservoirs")    
    
## LOAD BEND DATA 
bends<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    "bends_sp_ll")
 
 
 
plot(bends,axes=TRUE)
plot(reservoirs,add=TRUE)
plot(manunits,add=TRUE,col='red')
points(y~x,hatcheries,pch=19)




