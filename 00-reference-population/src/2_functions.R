
# FUNCTION TO DISTRIBUTE FISH AMONG SEGEMENTS AND THEN BENDS
allocate_pallids<- function(segment=1,
    fish_density=1,type=NULL)
    {
    # this function allocates fish to bends within a segment
    # probabilistically given bend weights
    
    # inputs
    ## segment: segment [1,2,3,4,7,8,9,10,13,14]
    ## fish_density: density of fish within segment; fish/rkm
    ## type: input for fish type [hatchery, natural]
    
    
    # GET BEND INFORMATION
    tmp<- subset(bends, b_segment==segment)
    # PLACEHOLDER TO STORE FISH NUMBERS
    tmp$pallids<- 0
    
    # PULL NUMBER FROM A POISSON AFTER ADJUSTING
    # DENSITY FOR BEND SIZE
    tmp$pallids<- rpois(nrow(tmp),fish_density*tmp$length.rkm)
    indx<- match(c("b_segment","bend_num","length.rkm",
        "basin","state","bend_num","pallids"),names(tmp))
    return(tmp[,indx])# return relvant stuff
    }
    
    
   


















## 
ini_age<- function(len,linf,k,sizeAtHatch=7,maxAge)
	{
	age<-ifelse(len==0,0,log(-1*(len-sizeAtHatch)/(linf-sizeAtHatch)+1)/-k)
	age<- ifelse(age>maxAge,maxAge,age)
	return(age*12)	
	}
	


## INITIALIZE-SPATIAL-LOCATION.R
initialize_spatial_location<- function(n,nbends,relativeDensity)
	{
	x<- sample(c(1:nbends),n,relativeDensity,replace=TRUE)
	return(x)
	}

## 001-adult-survival.R from population model
dSurvival<- function(phi_age,age,maxAge)
	{
	# phi_age: vector of age specific survival
	# age: vector of age in months for live individuals
	phi<- c(phi_age^(1/12))# convert annual to monthly, add 0 to zero out survival of unalive fish
	a<- floor(age/12)
	out<- rbinom(length(a),1,phi[a])
	out<- ifelse(out>maxAge,0,out) #SENESCENCE
	return(out)
	}