## Population Simulations
  segs<- c(1,2,3,4,7,8,9,10,13,14)
  nyears<- 10

  beta0<- 2.9444
  phi<-matrix(plogis(beta0),length(segs),nyears-1)

  fish_density<- 10

  sim_pop<-reference_population(segs=segs,
                                bends=bends,# BENDS DATAFRAME
                                fish_density=10, # FISH DENSITY PER RKM
                                #nyears=nyears, #NUMBER OF YEARS TO PROJECT
                                phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

  head(sim_pop) 


## Catch Simulations
  sim_catch<-catch_counts(segs=segs,
                          bends=bends,
                          n=sim_pop,
                          effort=effort)

  head(sim_catch)
  sim_catch[1:6,1,1]
  dim(sim_catch)
    #NOTE: sim_catch[i,j,k] will give the catch for bend i, in year j, when using gear k.
    #See function description for list of gears k.
  
  
#Sampling Simulation
  sim_samp<-bend_samples(segs=segs,bends=bends,n=sim_pop)
  nrow(sim_pop)*ncol(sim_pop)==nrow(sim_samp)
  head(sim_samp)
  str(sim_samp)
  
  
#SAMPLING OUTPUT 
  segs<- c(1,2,3,4,7,8,9,10,13,14)
  nyears<- 10
  
  beta0<- 2.9444
  phi<-matrix(plogis(beta0),length(segs),nyears-1)
  
  sim_data<-samp_dat(segs=segs,
            bends=bends,# BENDS DATAFRAME
            fish_density=10, # FISH DENSITY PER RKM
            nyears=nyears, #NUMBER OF YEARS TO PROJECT
            phi=phi,
            effort=effort)
  head(sim_data)

CPUE<-C/f

# CATCH EFFORT
p<-0.05 # capture p

catch<- rbinom(1,N,p)
ind<- rep(1,N)

ch<- rbinom(N,1,p)
ch<- cbind(ch,  rbinom(N,1,p))
