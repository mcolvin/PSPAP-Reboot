# PRADEL ANALYSIS

loc<-"D:/"
setwd("D:/_output/6-MARK/Pradel/MARK")

samp_type<-"r"
ptm<-proc.time()
lapply(201:210, function(i)
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
        if(file.exists(paste0(loc, "_output/3-estimates/Pradel_est_", samp_type, "_",
                              i, "-", j,".rds")))
        {
        old<-readRDS(file=paste0(loc, "_output/3-estimates/Pradel_est_", samp_type, "_",
                                 i, "-", j, ".rds"))
        est$const<-rbind(old$const,est$const)
        est$p.time<-rbind(old$p.time,est$p.time)
        est$f.time<-rbind(old$f.time,est$f.time)
        rm(old)
      }
      saveRDS(est, file=paste0(loc,"_output/3-estimates/RD_est_", samp_type, "_", i,
                               "-", j, ".rds"))
      }
    rm(est)
    })
  })
})
proc.time()-ptm
