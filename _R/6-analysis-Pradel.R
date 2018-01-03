# PRADEL ANALYSIS

source("_R/1_global.R")

loc<-"D:/"
setwd("D:/_output/6-MARK/Pradel/MARK")

samp_type<-"r"
ptm<-proc.time()
lapply(201:202, function(i)
{
  lapply(3:4, function(j)
  {
    # SET OCCASIONS TO BE USED
    occasions<-1:4
    lapply(occasions, function(y)
    {
      # GET PRADEL ESTIMATES
      est<-try(Pradel.ests(pop_num=i, catch_num = j, samp_type = samp_type,
                           location = loc, max_occ=y), silent=TRUE)
      # SAVE ESTIMATES
      if(class(est)!="try-error")
      {
        if(file.exists(paste0(loc, "_output/3-estimates/Pradel_est_", est_type, "_",
                              samp_type, "_", i, "-", j,".rds")))
        {
        old<-readRDS(file=paste0(loc, "_output/3-estimates/Pradel_est_", est_type, "_", 
                                 samp_type, "_", i, "-", j, ".rds"))
        est<-rbind(old,est)
        rm(old)
      }
      saveRDS(est, file=paste0(loc,"_output/3-estimates/Pradel_est_", est_type, "_", 
                               samp_type, "_", i, "-", j, ".rds"))
      }
    rm(est)
    })
  })
})
proc.time()-ptm
