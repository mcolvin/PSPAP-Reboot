
segs<- c(1,2,3,4,7,8,9,11,10,13,14)
nyears<- 20
beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

fish_density<- 10


sim_pop<-reference_population(segs=segs,
    bends=bends,# BENDS DATAFRAME
    fish_density=10, # FISH DENSITY PER RKM
    phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

    
    
    
    