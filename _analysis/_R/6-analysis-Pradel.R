# PRADEL ANALYSIS

source("_R/1_global.R")

#216-3 occ4 needs to be finished (see Pradel folder)
#CHANGED FOR 217-3+ to j<-3 and occasions 2:3
#222-3 needs occ3 ran and occ2 added
#CHANGED FOR 286-3+ to only ftime estimates
#379-4 f NEEDS TO BE FINISHED
#218-3 occ1 NEEDS TO BE ADDED
#218-285 3 are lists of 6 and not estimates that can be combined...
#285 needs all

loc<-"D:/"
setwd("D:/_output/6-MARK/Pradel/MARK")

#check<-readRDS("D:/_output/3-estimates/Pradel_est_r_216-3.rds")

samp_type<-"f"
ptm<-proc.time()
lapply(1:400, function(i)
{
  lapply(3:4, function(j)
  {
    # SET OCCASIONS TO BE USED
    occasions<-c(1,4)
    lapply(occasions, function(y)
    {
      # GET PRADEL ESTIMATES
      est<-try(Pradel.ests(pop_num=i, catch_num = j, samp_type = samp_type,
                           location = loc, max_occ=y), silent=TRUE)
      # SAVE ESTIMATES
      if(class(est)!="try-error")
      {
        if(file.exists(paste0(loc, "_output/3-estimates/Pradel_est_",
                              samp_type, "_", i, "-", j,".rds")))
        {
          old<-readRDS(file=paste0(loc, "_output/3-estimates/Pradel_est_", 
                                   samp_type, "_", i, "-", j, ".rds"))
          est<-rbind(old,est)
          rm(old)
        }
        saveRDS(est, file=paste0(loc,"_output/3-estimates/Pradel_est_", 
                                 samp_type, "_", i, "-", j, ".rds"))
      }
      rm(est)
    })
  })
})
proc.time()-ptm
