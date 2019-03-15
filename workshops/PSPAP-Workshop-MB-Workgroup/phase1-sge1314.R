
comm<-odbcConnectAccess("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/20171103-pallid-dbase.mdb")




dat<- sqlFetch(comm,"seg1314-pilot")
names(dat)<-tolower(names(dat))

indx<-c(grep("GN",unique(dat$gear)),grep("TL",unique(dat$gear)))
grs<-unique(dat$gear)[indx]





Combo 1. Trotlines 3 days
COmbo 2. Gillnets 3 days
Combo 3. Gillnets (0.5) and trotlines (0.5)
Combo 4. Gillnets 

Targetting versus random for gear deployment. 


Saturated --> short bends

Josh Schloesser --> number of hooks per river mile to detect pallid in a bend. 

SOP... 3rd week of March--> 1 Month