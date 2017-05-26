
# READ IN BEND DATA FOR RPMA 2 (UPPER) AND 4 (LOWER)

  bends<- read.csv("./dat/bend-data.csv")

  # make data.frame column names lower case
  names(bends)<- tolower(names(bends))
  bends<- subset(bends,b_segment %in% c(1,2,3,4,7,8,9,10,13,14))



  # READ IN EFFORT DATA FROM 01-PSPAP-Background Analysis
  
  effort<- read.table("./dat/effort_dat.csv")
  
  #Report Effort for GN14,GN18,GN41, GN81, MF, OT16, TLC1, TLC2, TN for both LB and UB
  effort<-rbind(effort[1:8,],effort[12:13,],effort[2,], effort[14,], effort[4,],effort[15:18,],effort[20,])
  effort[11,1]<-"UB"
  effort[13,1]<-"UB"
  effort[11,4:11]<-as.integer(rep(0,8))
  effort[13,4:11]<-as.integer(rep(0,8))
  
  
  # READ IN DENSITY DATA
  dens<-read.csv("./dat/fish_density.csv")


