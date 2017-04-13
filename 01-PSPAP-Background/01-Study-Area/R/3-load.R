
# CHECK TO SEE IF DATA HAS ALREAD BEEN LOADED AND CLEANED
if("checksums.rds" %in% dir("./output"))
    {checksums<- loadRds("./output/checksums")}else
    {checksums<- list()}

# MISSOURI RIVER WATERSHES

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
saveRDS(missouri_ws,"./output/missouri-watershed.rds")



# LOAD AND CLEAN COVERAGE OF US STATES
states<- readOGR("C:/Users/mcolvin/My Documents/projects/gis coverages", "statep010")
## SUBSET CONTIGUOUS US STATES
states<- subset(states, !(states@data$STATE %in% c("Alaska", "Hawaii", "Puerto Rico",
	"U.S. Virgin Islands")))
## REMOVE SOME ISLANDS
states<- subset(states,!(AREA %in% c(1.166,0.994,0.790,11.527,2.739,0.443,0.065,0.211)))
## EXTRACT COORDINES FOR EACH STATE CENTROID
lps<- coordinates(states)
## MAKE AN ID TO DISSOLVE HUCS WITH
ID<- rep(1, nrow(lps))
# DISSOLVE STATES FOR OUTLINE OF U.S.
states_dissolve <- unionSpatialPolygons(states,ID)	
	

# PALLID STURGEON MANAGEMENT UNITS

## LOAD DATA
manunits<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
    "PallidSturgeon_ManagementUnits")
##

plot(manunits[manunits$Name=="Coastal Plains",])    
plot(manunits[manunits$Name=="Central Lowlands",])    
    
    
# MISSOURI RIVER SPECIFIC COVERAGES
lmo_miss<- readOGR("C:/Users/My Document/projects/Pallid Sturgeon/data/GIS",
    "Lower_Mo_River_And_Miss",verbose=FALSE)
# lmo<- readOGR("C:/Users/My Document/projects/Pallid Sturgeon/data/GIS", 
#    "Lower_Mo_River_No_Miss")
#
#umo_yellowstone<- readOGR("C:/Users/My Document/projects/Pallid Sturgeon/data/GIS", 
#    "upper_mo_river")
#states<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/analysis/dat", "states")
#wb<- readOGR("C:/Users/mcolvin/My Document/projects/Pallid Sturgeon/data/GIS", 
 #   "water_bodies_mo_river")
#umo<- readOGR("C:/Users/mcolvin/My Document/projects/Pallid Sturgeon/data/GIS", 
#    "umo_to_intake1")

    
# HATCHERY DATA
chan<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")
hatcheries<- sqlFetch(chan, "Hatchery_latlongs")

# LOAD MAJOR STREAMS - MAY BE REDUNDANCE WITH ABOVE, CONSOLIDATING CODE
majorstreams<- readOGR("C:/Users/mcolvin/My documents/projects/gis coverages/National Coverages", 
    "hydrogl020")

    
    
# WATER BODIES 

## LOAD DATA
wb<- readOGR("C:/Users/mcolvin/My Documents/projects/gis coverages/National Coverages", 
    "wtrbdyp010")
## SUBSET OUT MISSOURI RIVER RESERVOIRS
reservoirs<- subset(wb, Name %in%c("Lake Sakakawea",
    "Lake Francis Case","Lake Oahe", "Lake Sharpe", "Fort Peck Lake",
    "Lewis and Clark Lake"))

plot(bends)
## LOAD BEND DATA 

bends<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    "bends_sp_ll")
    
    
    
plot(bends,axes=TRUE)
plot(reservoirs,add=TRUE)
points(y~x,hatcheries,pch=19)




