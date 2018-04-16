# RD ANALYSIS

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

#########################################################
# 1-2 Occ3, 2-3 all, & 2-1 all, do not have "fit" saved #
#########################################################
# NEED 57-4 #  
###########################################################
# 275 Occ2 UB needs to be added to final estimator output #
###########################################################
# NEED TO RUN 293-4 occ3 LB #
#############################


if(pcname=="WF-FGNL842")
{
  loc<-"E:/"
}
if(pcname!="WF-FGNL842")
{
  loc<-"D:/"
}

setwd("E:/_output/6-MARK/MARK")

ptm<-proc.time()
lapply(337:400, function(i)
{
  lapply(3:4, function(j)
  {
  # SET OCCASIONS TO BE USED
  occasions<-2:3
  lapply(occasions, function(y)
  {
    basin<-c("U", "L")
    lapply(basin, function(b)
    {
      g<-"TLC1"
      #gears<-c("GN14","TLC1", "TN")
      #lapply(gears, function(g)
      #{
      # GET RD ESTIMATES
      est<-try(RD_r.ests(basin_code=b, pop_num=i, catch_num = j, location = loc,
                       max_occ=y, gear_combi=g), silent=TRUE)
    # SAVE ESTIMATES
    if(class(est)!="try-error")
      {
      saveRDS(est, file=paste0(loc,"_output/3-estimates/RD_est_r_", i, "-", j,
                               "_occ-", y, "_", b, "B-",g,".rds"))
      }
    #     if(file.exists(paste0(loc, "_output/3-estimates/RD_est_r_", i, "-", j,
    #                         ".rds")))
    #   {
    #     old<-readRDS(file=paste0(loc, "_output/3-estimates/RD_est_f_", i, "-",
    #                              j, ".rds"))
    #     est$ests<-rbind(old$ests,est$ests)
    #     est$COMBI<-rbind(old$COMBI,est$COMBI)
    #     est$parameters<-rbind(old$parameters,est$parameters)
    #     est$model<-rbind(old$model,est$model)
    #     rm(old)
    #   }
    #   saveRDS(est, file=paste0(loc,"_output/3-estimates/RD_est_f_", i, "-", j,
    #                            ".rds"))
    # }
      rm(est)
    })
  })
  })
})
proc.time()-ptm
