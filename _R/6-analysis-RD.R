# RD ANALYSIS

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

ptm<-proc.time()
lapply(1:1, function(i)
{
  if(pcname=="WF-FGNL842")
  {
    catch_list<-dir("E:/_output/2-catch", pattern=paste0("catch_dat_f_",i,"-"))
  }
  if(pcname!="WF-FGNL842")
  {
    catch_list<-dir("D:/_output/2-catch", pattern=paste0("catch_dat_f_",i,"-"))
  }
  lapply(1:length(catch_list), function(j)
  {
    # READ IN DATA
    if(pcname=="WF-FGNL842")
    {sim_dat<-readRDS(file=paste0("E:/_output/2-catch/",catch_list[j]))}
    if(pcname!="WF-FGNL842")
    {sim_dat<-readRDS(file=paste0("D:/_output/2-catch/",catch_list[j]))}
    # SET OCCASIONS TO BE USED
    occasions<-2:4
    lapply(occasions, function(y)
    {
      # GET RD ESTIMATES
      est<-RD.ests(sim_dat=sim_dat, max_occ=y)
      # SAVE ESTIMATES
      if(pcname=="WF-FGNL842")
      {
        if(file.exists(paste0("E:/_output/3-estimates/RD_est", 
                              strsplit(catch_list[j], "catch_dat")[[1]][2])))
        {
          old<-readRDS(file=paste0("E:/_output/3-estimates/RD_est", 
                                   strsplit(catch_list[j], "catch_dat")[[1]][2]))
          est<-rbind(old,est)
        }
        saveRDS(est,
                file=paste0("E:/_output/3-estimates/RD_est", 
                            strsplit(catch_list[j], "catch_dat")[[1]][2]))
      }
      if(pcname!="WF-FGNL842")
      {
        if(file.exists(paste0("D:/_output/3-estimates/RD_est", 
                              strsplit(catch_list[j], "catch_dat")[[1]][2])))
        {
          old<-readRDS(file=paste0("D:/_output/3-estimates/RD_est", 
                                   strsplit(catch_list[j], "catch_dat")[[1]][2]))
          est<-rbind(old,est)
        }
        saveRDS(est,
                file=paste0("D:/_output/3-estimates/RD_est", 
                            strsplit(catch_list[j], "catch_dat")[[1]][2]))
      }
    })
  })
})
proc.time()-ptm