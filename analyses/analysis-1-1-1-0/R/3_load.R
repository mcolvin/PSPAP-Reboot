

# LOAD PREVIOUSLY RUN SIMULATION RESULTS	
sims<-dir("./output")

if("simulation-results.RDS" %in% sims)
    {
    out<- readRDS("./output/simulation-results.RDS")
    }
    
if("BDN-results.RDS"%in% sims)
    {    
    out_bdn<- readRDS("./output/BDN-results.RDS")
    }
