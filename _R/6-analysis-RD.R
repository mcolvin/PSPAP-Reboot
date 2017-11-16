# RD ANALYSIS

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

#############################################################################
# 1-2 HAS OCC 3 ONLY; 1-2 Occ3, 2-3 all, & 2-1 all, do not have "fit" saved #
#############################################################################


if(pcname=="WF-FGNL842")
{
  loc<-"E:/"
}
if(pcname!="WF-FGNL842")
{
  loc<-"D:/"
}
ptm<-proc.time()
lapply(1:400, function(i)
{
  lapply(1:4, function(j)
  {
    # SET OCCASIONS TO BE USED
    occasions<-2:3
    lapply(occasions, function(y)
    {
      # GET RD ESTIMATES
      est<-try(RD.ests(pop_num=i, catch_num = j, location = loc, max_occ=y), silent=TRUE)
      # SAVE ESTIMATES
      if(pcname=="WF-FGNL842")
      {
        if(file.exists(paste0("E:/_output/3-estimates/RD_est", 
                              strsplit(catch_list[j], "catch_dat")[[1]][2])))
        {
          old<-readRDS(file=paste0("E:/_output/3-estimates/RD_est", 
                                   strsplit(catch_list[j], "catch_dat")[[1]][2]))
          est$ests<-rbind(old$ests,est$ests)
          est$COMBI<-rbind(old$COMBI,est$COMBI)
          est$parameters<-rbind(old$parameters,est$parameters)
          est$model<-rbind(old$model,est$model)
          rm(old)
        }
        if(class(est)!="try-error")
        {saveRDS(est,
                 file=paste0("E:/_output/3-estimates/RD_est", 
                             strsplit(catch_list[j], "catch_dat")[[1]][2]))}
      }
      if(pcname!="WF-FGNL842")
      {
        if(file.exists(paste0("D:/_output/3-estimates/RD_est", 
                              strsplit(catch_list[j], "catch_dat")[[1]][2])))
        {
          old<-readRDS(file=paste0("D:/_output/3-estimates/RD_est", 
                                   strsplit(catch_list[j], "catch_dat")[[1]][2]))
          est$ests<-rbind(old$ests,est$ests)
          est$COMBI<-rbind(old$COMBI,est$COMBI)
          est$parameters<-rbind(old$parameters,est$parameters)
          est$model<-rbind(old$model,est$model)
          rm(old)
        }
        if(class(est)!="try-error")
        {saveRDS(est,
                 file=paste0("D:/_output/3-estimates/RD_est", 
                             strsplit(catch_list[j], "catch_dat")[[1]][2]))}
      }
      rm(est)
    })
  })
  #stopCluster(cl)
})
proc.time()-ptm
