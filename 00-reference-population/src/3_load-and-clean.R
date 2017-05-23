
# READ IN BEND DATA FOR RPMA 2 (UPPER) AND 4 (LOWER)

bends<- read.csv("./dat/bend-data.csv")

# make data.frame column names lower case
names(bends)<- tolower(names(bends))
bends<- subset(bends,b_segment %in% c(1,2,3,4,7,8,9,10,13,14))
