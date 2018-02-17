

segs<-data.frame(
    basin=c(2,2,2,2,4,4,4,4,4,4),
    segment=c(1,2,3,4,
        7,8,9,10,13,14),
    nbends=c(1,40,91,24,
        34,61,81,39,45,56))
prop_sampled<-c(0.05,0.1,0.15,0.2,0.25)

out<-matrix(0,10,length(prop_sampled))
for(i in 1:length(prop_sampled))
    {
    out[,i]<- round(segs$nbends*prop_sampled[i],0)
    }

colSums(out[which(segs$basin==2),])    
colSums(out[which(segs$basin==4),])    
    

