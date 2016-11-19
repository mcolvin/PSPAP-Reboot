

# BASE GIS COVERAGES
hucs<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "hucs00p020")
states<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "statep010")

# MISSOURI RIVER SPECIFIC COVERAGES
lmo_miss<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "Lower_Mo_River_And_Miss")
lmo<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "Lower_Mo_River_No_Miss")
manunits<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "Pallid_Sturgeon_management_units")
umo_yellowstone<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "upper_mo_river")
#states<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/analysis/dat", "states")
wb<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "water_bodies_mo_river")
umo<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "umo_to_intake1")

chan<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Data/pallids.accdb")
hatcheries<- sqlFetch(chan, "Hatchery_latlongs")

