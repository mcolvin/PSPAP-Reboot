# RD ANALYSIS

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

ptm<-proc.time()
lapply(400:398, function(i)
{
  if(pcname=="WF-FGNL842")
  {
    catch_list<-dir("E:/_output/2-catch", pattern=paste0("catch_dat_f_",i,"-"))
  }
  if(pcname!="WF-FGNL842")
  {
    catch_list<-dir("D:/_output/2-catch", pattern=paste0("catch_dat_f_",i,"-"))
  }
  # library(parallel)
  # ## USE ALL CORES
  # numCores<-detectCores()
  # ## INITIATE CLUSTER
  # cl<-makeCluster(numCores)
  # ## MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
  # clusterExport(cl, c("pcname","catch_list"),envir=environment())
  # clusterEvalQ(cl, source("_R/2_functions.R"))
  # clusterEvalQ(cl, library(plyr))
  # clusterEvalQ(cl, library(reshape2))
  # parLapply(cl,1:length(catch_list), function(j)
  lapply(1:4, function(j)
  {
    # READ IN DATA
    if(pcname=="WF-FGNL842")
    {sim_dat<-readRDS(file=paste0("E:/_output/2-catch/",catch_list[j]))}
    if(pcname!="WF-FGNL842")
    {sim_dat<-readRDS(file=paste0("D:/_output/2-catch/",catch_list[j]))}
    # SET OCCASIONS TO BE USED
    occasions<-2:3
    lapply(occasions, function(y)
    {
      # GET RD ESTIMATES
      est<-try(RD.ests(sim_dat=sim_dat, max_occ=y), silent=TRUE)
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
