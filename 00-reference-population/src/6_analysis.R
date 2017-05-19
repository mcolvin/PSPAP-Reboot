
segs<- c(1,2,3,4,7,8,9,11,10,13,14)
nyears<- 20

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

fish_density<- 10


sim_pop<-reference_population(segs=segs,
    bends=bends,# BENDS DATAFRAME
    fish_density=10, # FISH DENSITY PER RKM
    phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

    
head(sim_pop)  

f<- 200 # trammel
q<- 0.01
N<-sim_pop[1,1]  

C<- f*q*N    
C
C/f


# CATCH EFFORT
p<-0.05 # capture p

catch<- rbinom(1,N,p)
ind<- rep(1,N)

ch<- rbinom(N,1,p)
ch<- cbind(ch,  rbinom(N,1,p))
