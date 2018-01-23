gear	q_min	q_max
GN14	0	5.00E-05
GN18	0	5.00E-05
GN41	0	5.00E-05
GN81	0	5.00E-05
MF	0	5.00E-05
OT16	5.00E-04	0.0065
TLC1	0	5.00E-05
TLC2	0	5.00E-05
TN	1.00E-04	0.006

dat<- data.frame(gear=c("Gill nets","Trot lines","Trammel nets"),
    q_min=c(0,0,0),
    q_max=c(0.00005,0.00005,0.006),
    q_sd_min=c(0,0,0),
    q_sd_max=c(1.5,1.5,1.5))


N<- 500000

sd_bins<- lapply(1:nrow(dat),function(x)
    {
    seq(dat[x,]$q_sd_min,dat[x,]$q_sd_max,length.out=5)
    })

out<-expand.grid(gear=dat$gear,rep=1:20000)



out$catchability<-0
out$B0_sd<-0
for(gear in as.character(dat$gear))
    {
    indx<- which(out$gear==gear)
    out[indx,]$catchability<-runif(length(indx),
        dat[dat$gear==gear,]$q_min, 
        dat[dat$gear==gear,]$q_max)
     out[indx,]$B0_sd<-runif(length(indx),
        dat[dat$gear==gear,]$q_sd_min, 
        dat[dat$gear==gear,]$q_sd_max)       
    }
    
    

out$B0<- log(out$catchability/(1-out$catchability))

out$catchability_i<- plogis



   
    
    
    
    