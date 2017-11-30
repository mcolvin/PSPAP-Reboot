# RD ANALYSIS

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

### NEED TO FIX 396-4 and 395-4

if(pcname=="WF-FGNL842")
{
  loc<-"E:/"
}
if(pcname!="WF-FGNL842")
{
  loc<-"D:/"
}
ptm<-proc.time()
lapply(394:390, function(i)
{
  lapply(3:4, function(j)
  {
    # SET OCCASIONS TO BE USED
    occasions<-2:3
    lapply(occasions, function(y)
    {
      # GET RD ESTIMATES
      est<-try(RD.ests(pop_num=i, catch_num = j, location = loc, max_occ=y), silent=TRUE)
      # SAVE ESTIMATES
      if(class(est)!="try-error")
      {
        if(file.exists(paste0(loc, "_output/3-estimates/RD_est_f_", i, "-", j,
                              ".rds")))
        {
          old<-readRDS(file=paste0(loc, "_output/3-estimates/RD_est_f_", i, "-",
                                   j, ".rds"))
          est$ests<-rbind(old$ests,est$ests)
          est$COMBI<-rbind(old$COMBI,est$COMBI)
          est$parameters<-rbind(old$parameters,est$parameters)
          est$model<-rbind(old$model,est$model)
          rm(old)
        }
        saveRDS(est, file=paste0(loc,"_output/3-estimates/RD_est_f_", i, "-", j,
                                 ".rds"))
      }
      rm(est)
    })
  })
})
proc.time()-ptm
