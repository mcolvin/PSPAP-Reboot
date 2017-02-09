



com<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PDSG-LW/dat/dat.accdb")

dat<- sqlFetch(com,"segment-bend-sampling")



