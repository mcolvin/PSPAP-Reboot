#1. REMOVE REPEAT DATA
#   A. CHECK & REPLACE KNOWN REPEATS
#   B. RUN THROUGH ALL AND REMOVE REPEATS SIMULTANEOUSLY
#2. MAKE ABUNDANCE, TREND, AND LENGTH TABLE


source("_R/1_global.r")
source("_R/2_functions.r")

##########################################
#         1. REMOVE REPEAT DATA          #
##########################################
## A.
# FIND REPEATS
ptm<-proc.time()
repeats<-lapply(1:200, function(i)
{
  if(pcname=="WF-FGNL842")
  {
    est_list<-dir("E:/_output/3-estimates", pattern=paste0("_est_r_",i,"-"))
    est_list<-c(est_list,dir("E:/_output/3-estimates", 
                             pattern=paste0("_est_f_",i,"-")))
  }
  if(pcname!="WF-FGNL842")
  {
    est_list<-dir("D:/_output/3-estimates", pattern=paste0("_est_r_",i,"-"))
    est_list<-c(est_list,dir("D:/_output/3-estimates", 
                             pattern=paste0("_est_f_",i,"-")))
    
  }
  library(parallel)
  ## USE ALL CORES
  numCores<-detectCores()
  ## INITIATE CLUSTER
  cl<-makeCluster(numCores)
  ## MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
  clusterExport(cl, c("pcname","est_list"),envir=environment())
  out<-parLapply(cl, 1:length(est_list), function(j)
  {
    outt<-NULL
    if(pcname=="WF-FGNL842")
    {
      est<-readRDS(file=paste0("E:/_output/3-estimates/", est_list[j]))
    }
    if(pcname!="WF-FGNL842")
    {
      est<-readRDS(file=paste0("D:/_output/3-estimates/", est_list[j]))
    }
    if(anyDuplicated(est$ests)>0){outt<-est_list[j]}
    if(anyDuplicated(est$COMBI)>0){outt<-est_list[j]}
    return(outt)
  })
  stopCluster(cl)
  out<-do.call("rbind",out)
  return(out)
})
repeats<-do.call("rbind", repeats)
proc.time()-ptm
# FOR HALF THE DATA:
# user    system    elapsed 
# 8.25    4.93      1461.65

# REMOVE REPEATS FOUND ABOVE
library(parallel)
## USE ALL CORES
numCores<-detectCores()
## INITIATE CLUSTER
cl<-makeCluster(numCores)
## MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
clusterExport(cl, c("pcname","repeats"),envir=environment())
parLapply(cl, 1:length(repeats), function(j)
{
  if(pcname=="WF-FGNL842")
  {
    est<-readRDS(file=paste0("E:/_output/3-estimates/", repeats[j]))
    est$ests<-est$ests[!duplicated(est$ests),]
    if(!is.null(est$COMBI)){est$COMBI<-est$COMBI[!duplicated(est$COMBI),]}
    saveRDS(est,file=paste0("E:/_output/3-estimates/", repeats[j]))
  }
  if(pcname!="WF-FGNL842")
  {
    est<-readRDS(file=paste0("D:/_output/3-estimates/", repeats[j]))
    est$ests<-est$ests[!duplicated(est$ests),]
    if(!is.null(est$COMBI)){est$COMBI<-est$COMBI[!duplicated(est$COMBI),]}
    saveRDS(est,file=paste0("D:/_output/3-estimates/", repeats[j]))
  }
})
stopCluster(cl)


# ##B.
# ptm<-proc.time()
# lapply(1:400, function(i)
# {
#   if(pcname=="WF-FGNL842")
#   {
#     est_list<-dir("E:/_output/3-estimates", pattern=paste0("_est_r_",i,"-"))
#     est_list<-c(est_list,dir("E:/_output/3-estimates", 
#                              pattern=paste0("_est_f_",i,"-")))
#   }
#   if(pcname!="WF-FGNL842")
#   {
#     est_list<-dir("D:/_output/3-estimates", pattern=paste0("_est_r_",i,"-"))
#     est_list<-c(est_list,dir("D:/_output/3-estimates", 
#                              pattern=paste0("_est_f_",i,"-")))
#     
#   }
#   library(parallel)
#   ## USE ALL CORES
#   numCores<-detectCores()
#   ## INITIATE CLUSTER
#   cl<-makeCluster(numCores)
#   ## MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
#   clusterExport(cl, c("gear_codes","est_list"),envir=environment())
#   parLapply(cl, 1:length(est_list), function(j)
#   {
#     if(pcname=="WF-FGNL842")
#     {
#       est<-readRDS(file=paste0("E:/_output/3-estimates/", est_list[j]))
#       est$ests<-est$ests[!duplicated(est$ests),]
#       saveRDS(est,file=paste0("E:/_output/3-estimates/", est_list[j]))
#     }
#     if(pcname!="WF-FGNL842")
#     {
#       est<-readRDS(file=paste0("D:/_output/3-estimates/", est_list[j]))
#       est$ests<-est$ests[!duplicated(est$ests),]
#       saveRDS(est,file=paste0("D:/_output/3-estimates/", est_list[j]))
#     }
#     if(!is.null(est$COMBI)){return(print("COMBI not NULL."))}
#   })
#   stopCluster(cl)
# })
# proc.time()-ptm


##########################################
#         2. MAKE OUTPUT TABLES          #
##########################################

# MAKE OUTPUT TABLES FROM ESTIMATES AND SAVE MINI TABLES ALONG THE WAY
samp_type<-"r"
if(pcname=="WF-FGNL842"){loc<-"E:/"}
#if(pcname!="WF-FGNL842"){loc<-"D:/"}
if(pcname!="WF-FGNL842"){loc<-NULL}
outi<-lapply(1:200, function(i)
{
  # RUN IN PARALLEL
  library(parallel)
  ## USE ALL CORES
  numCores<-detectCores()
  ## INITIATE CLUSTER
  cl<-makeCluster(numCores)
  ## MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
  clusterExport(cl, c("samp_type", "loc","i"),envir=environment())
  clusterEvalQ(cl, source("_R/2_functions.R"))
  clusterEvalQ(cl, library(plyr))
  clusterEvalQ(cl, library(reshape2))
  outj<-parLapply(cl,1:4, function(j)
  {
    ############################################################
    # ADD RECRUITMENT TO abund.trnd PRIOR TO RUNNING AGAIN!!!! #
    ############################################################
    atl<-abund.trnd(samp_type=samp_type, pop_num=i, catch_num=j,location=loc)
    sim_dat<-readRDS(file=paste0(loc,"_output/2-catch/catch_dat_",samp_type,"_",i,"-",j,".rds"))
    atl$lgth<-length.dat(sim_dat = sim_dat)
    # ADD REFERENCE COLUMNS TO LENGTH DATA
    atl$lgth$pop_id<-i
    atl$lgth$catch_id<-j
    atl$lgth<-atl$lgth[,c(1:18,20,21,19)]
    # SAVE MINIS
    write.csv(atl$trnd, file=paste0(loc,"_output/4-tables/trnd_mini_",samp_type,"_",i,"-",j,".csv"),row.names=FALSE)
    write.csv(atl$abund, file=paste0(loc,"_output/4-tables/abund_mini_",samp_type,"_",i,"-",j,".csv"),row.names=FALSE)
    write.csv(atl$lgth, file=paste0(loc,"_output/4-tables/lgth_mini_",samp_type,"_",i,"-",j,".csv"),row.names=FALSE)
    return(atl)
  })
  ### CLOSE CLUSTERS
  stopCluster(cl)
  trnd<-do.call(rbind,lapply(outj, `[[`, 1))
  abund<-do.call(rbind,lapply(outj, `[[`, 2))
  COMBI<-do.call(rbind,lapply(outj, `[[`, 3))
  lgth<-do.call(rbind,lapply(outj, `[[`, 4))
  return(list(trnd=trnd, abund=abund, COMBI=COMBI, lgth=lgth))
})

# SAVE COMPILED TABLES & MOVE MINIS TO COMPILED FOLDER
## COMPILE NEW RESULTS
trnd<-do.call(rbind,lapply(outi, `[[`, 1))
abund<-do.call(rbind,lapply(outi, `[[`, 2))
COMBI<-do.call(rbind,lapply(outi, `[[`, 3))
if(ncol(COMBI)==0){COMBI<-NULL}
lgth<-do.call(rbind,lapply(outi, `[[`, 4))
## ADD TO MASTER TABLE
### IF PREVIOUS TABLE EXISTS
#### TREND
if(file.exists(paste0(loc, "_output/4-tables/trnd_table.csv")))
{
  trnd_table<-readRDS(file=paste0(loc, "_output/4-tables/trnd_table.rds"))
  trnd_table<-rbind(trnd_table,trnd)
  saveRDS(trnd_table,file=paste0(loc, "_output/4-tables/trnd_table.rds"))
  ## CHECK FOR DUPLICATE DATA
  anyDuplicated(trnd_table)
}
#### ABUNDANCE
if(file.exists(paste0(loc, "_output/4-tables/abund_table.csv")))
{
  abund_table<-readRDS(file=paste0(loc, "_output/4-tables/abund_table.rds"))
  abund_table<-rbind(abund_table,abund)
  saveRDS(abund_table,file=paste0(loc, "_output/4-tables/abund_table.rds"))
  ## CHECK FOR DUPLICATE DATA
  anyDuplicated(abund_table)
}
#### LENGTH
if(file.exists(paste0(loc, "_output/4-tables/lgth_table.csv")))
{
  lgth_table<-readRDS(file=paste0(loc, "_output/4-tables/lgth_table.rds"))
  lgth_table<-rbind(lgth_table,lgth)
  saveRDS(lgth_table,file=paste0(loc, "_output/4-tables/lgth_table.rds"))
  ## CHECK FOR DUPLICATE DATA
  anyDuplicated(lgth_table)  
}
### IF NO PREVIOUS TABLE EXISTS
#### TREND
if(!file.exists(paste0(loc, "_output/4-tables/trnd_table.csv")))
{
  write.csv(trnd,file=paste0(loc, "_output/4-tables/trnd_table.csv"),row.names=FALSE)
  ## CHECK FOR DUPLICATE DATA
  anyDuplicated(trnd) 
}
#### ABUNDANCE
if(!file.exists(paste0(loc, "_output/4-tables/abund_table.csv")))
{
  write.csv(abund,file=paste0(loc, "_output/4-tables/abund_table.csv"),row.names=FALSE)
  ## CHECK FOR DUPLICATE DATA
  anyDuplicated(abund) 
}
#### LENGTH
if(!file.exists(paste0(loc, "_output/4-tables/lgth_table.csv")))
{
  write.csv(lgth,file=paste0(loc, "_output/4-tables/lgth_table.csv"),row.names=FALSE)
  ## CHECK FOR DUPLICATE DATA
  anyDuplicated(lgth) 
}
## MOVE NEW MINIS TO COMPILED FOLDER
new_minis<-dir(paste0(loc, "_output/4-tables"), pattern="_mini_")
lapply(new_minis, function(x)
{
  file.rename(from=paste0(loc, "_output/4-tables/", x), 
              to=paste0(loc, "_output/4-tables/compiled_minis/", x))
})



###################################
#  GIVEN AN OUTI FAIL, USE MINIS  #
###################################
# TREND
outi<-lapply(201:373, function(i)
{
  outj<-lapply(1:4, function(j)
  {
    trnd_new<-read.csv(file=paste0(loc, "_output/4-tables/trnd_mini_",
                                    samp_type,"_",i,"-",j,".csv"))
    return(trnd_new)
  })
  outj<-do.call(rbind,outj)
  return(outj)
})
outi<-do.call("rbind",outi)
trnd_table<-readRDS(file=paste0(loc, "_output/4-tables/trnd_table.rds"))
trnd_table<-rbind(trnd_table,outi)
trnd_table<-trnd_table[!duplicated(trnd_table),]
saveRDS(trnd_table,file=paste0(loc, "_output/4-tables/trnd_table.rds"))


# ABUNDANCE
outi<-lapply(201:373, function(i)
{
  outj<-lapply(1:4, function(j)
  {
    abund_new<-read.csv(file=paste0(loc, "_output/4-tables/abund_mini_",
                                    samp_type,"_",i,"-",j,".csv"))
    return(abund_new)
  })
  outj<-do.call(rbind,outj)
  return(outj)
})
outi<-do.call("rbind",outi)
abund_table<-readRDS("E:/_output/4-tables/abund_table.rds")
abund_table<-rbind(abund_table,outi)
abund_table<-abund_table[!duplicated(abund_table),]
saveRDS(abund_table,file=paste0(loc, "_output/4-tables/abund_table.rds"))






# LENGTH
outi<-lapply(201:373, function(i)
{
  outj<-lapply(1:4, function(j)
  {
    lgth_new<-read.csv(file=paste0(loc, "_output/4-tables/lgth_mini_",
                                    samp_type,"_",i,"-",j,".csv"))
    return(lgth_new)
  })
  outj<-do.call(rbind,outj)
  return(outj)
})
outi<-do.call(rbind,outi)    
lgth_table<-readRDS(file=paste0(loc, "_output/4-tables/lgth_table.rds"))
lgth_table<-rbind(lgth_table,outi)
lgth_table<-lgth_table[!duplicated(lgth_table),]
saveRDS(lgth_table,file=paste0(loc, "_output/4-tables/lgth_table.rds"))




# # ESTIMATOR RESULTS FOR CONDITIONAL PROBABILITY TABLES
# ## 1. TREND
# ## 2. ABUNDANCE
# 
# source("_R/1_global.R")
# source("_R/2_functions.R")
# source("_R/3_load-and-clean.R")
# 
# # TREND
# ## PULL CPUE TREND DATA
# trnd_files<-dir("D:/DataDump", pattern=c("cpue_trnd_"))
# ### USE ONLY DATA WITH PARTICULAR SAMPLING STRATEGY
# samp_type<-"r"
# trnd_files<-trnd_files[grep(paste0("_samp_type_",samp_type),trnd_files)]
# ## CREATE DATAFRAME
# trnd_cpue<-lapply(trnd_files, function(i)
# {
#   out<-readRDS(paste0("D:/DataDump/",i))
#   out$perform<-1
#   out$estimator<-"CPUE"
#   out<-out[,c("gear","trnd", "bias", "cv", "flags", "perform", "estimator",
#               "mean_q_input", "B0_sd_input","mean_q_realized", "q_sd_realized",
#               "deployments","occasions","total_effort")]
#   return(out)
# })
# trnd_cpue<-do.call("rbind",trnd_cpue)
# 
# ## PULL M0t TREND DATA
# trnd_files<-dir("D:/DataDump", pattern=c("M0t_trnd_"))
# ### USE ONLY DATA WITH PARTICULAR SAMPLING STRATEGY
# samp_type<-"r"
# trnd_files<-trnd_files[grep(paste0("_samp_type_",samp_type),trnd_files)]
# 
# ## CREATE DATAFRAME
# trnd_M0t<-lapply(trnd_files, function(i)
# {
#   out<-readRDS(paste0("D:/DataDump/",i))
#   out<-out$trnd_dat
#   out0A<-out[,c(1,3:5,21,11)]
#   out0A$estimator<-"M0_AM"
#   colnames(out0A)<-gsub("_AM_M0", "", colnames(out0A))
#   out0W<-out[,c(1,7:9,22,11)]
#   out0W$estimator<-"M0_WM"
#   colnames(out0W)<-gsub("_WM_M0", "", colnames(out0W))
#   outtA<-out[,c(1,12:14,23,20)]
#   outtA$estimator<-"Mt_AM"
#   colnames(outtA)<-gsub("_AM_Mt", "", colnames(outtA))
#   outtW<-out[,c(1,16:18,24,20)]
#   outtW$estimator<-"Mt_WM"
#   colnames(outtW)<-gsub("_WM_Mt", "", colnames(outtW))
#   out<-rbind(out0A, out0W,outtA, outtW)
#   out<-out[,c("gear","trnd", "bias", "cv", "flags", "perform", "estimator",
#               "mean_q_input", "B0_sd_input","mean_q_realized", "q_sd_realized",
#               "deployments","occasions","total_effort")]
#   return(out)
# })
# trnd_M0t<-do.call("rbind",trnd_M0t)
# 
# ## COMBINE CPUE AND M0t TREND ANALYSIS
# trnd_table<-rbind(trnd_cpue,trnd_M0t)
# 
# 
# # ABUNDANCE
# ## PULL CPUE ABUNDANCE DATA
# abund_files<-dir("D:/DataDump", pattern=c("cpue_abund_"))
# # USE ONLY DATA WITH PARTICULAR SAMPLING STRATEGY
# samp_type<-"r"
# abund_files<-abund_files[grep(paste0("_samp_type_",samp_type),abund_files)]
# # CREATE DATAFRAME
# abund_cpue<-lapply(abund_files, function(i)
# {
#   # ABUNDANCE
#   out<-readRDS(paste0("D:/DataDump/",i))
#   out$perform<-1
#   # ARITHMETIC MEAN
#   outA<-out[,c(1:3,5,8,10,12:19,21)]
#   colnames(outA)<-gsub("_AM", "", colnames(outA))
#   outA$estimator<-"CPUE_AM"
#   # WEIGHTED ARITHMETIC MEAN
#   outW<-out[,c(1:3,6,9,11,12:19,21)]
#   colnames(outW)<-gsub("_WM", "", colnames(outW))
#   outW$estimator<-"CPUE_WM"
#   out<-rbind(outA, outW)
#   out<-out[,c("b_segment","year","gear","Nhat", "bias", "cv", "flags", "perform", "estimator",
#               "mean_q_input", "B0_sd_input","mean_q_realized", "q_sd_realized",
#               "deployments","occasions","total_effort")]
#   return(out)
# })
# abund_cpue<-do.call("rbind",abund_cpue)
# 
# 
# ## PULL M0t ABUNDANCE DATA
# abund_files<-dir("D:/DataDump", pattern=c("M0t_abund_"))
# ### USE ONLY DATA WITH PARTICULAR SAMPLING STRATEGY
# samp_type<-"r"
# abund_files<-abund_files[grep(paste0("_samp_type_",samp_type),abund_files)]
# 
# ## CREATE DATAFRAME
# abund_M0t<-lapply(abund_files, function(i)
# {
#   out<-readRDS(paste0("D:/DataDump/",i))
#   out<-out$abund_dat
#   out0A<-out[,c(1,3:5,21,11)]
#   out0A$estimator<-"M0_AM"
#   colnames(out0A)<-gsub("_AM_M0", "", colnames(out0A))
#   out0W<-out[,c(1,7:9,22,11)]
#   out0W$estimator<-"M0_WM"
#   colnames(out0W)<-gsub("_WM_M0", "", colnames(out0W))
#   outtA<-out[,c(1,12:14,23,20)]
#   outtA$estimator<-"Mt_AM"
#   colnames(outtA)<-gsub("_AM_Mt", "", colnames(outtA))
#   outtW<-out[,c(1,16:18,24,20)]
#   outtW$estimator<-"Mt_WM"
#   colnames(outtW)<-gsub("_WM_Mt", "", colnames(outtW))
#   out<-rbind(out0A, out0W,outtA, outtW)
#   out<-out[,c("gear","Nhat", "bias", "cv", "flags", "perform", "estimator",
#               "mean_q_input", "B0_sd_input","mean_q_realized", "q_sd_realized",
#               "deployments","occasions","total_effort")]
#   return(out)
# })
# abund_M0t<-do.call("rbind",abund_M0t)
# 
# ## COMBINE CPUE AND M0t TREND ANALYSIS
# abund_table<-rbind(abund_cpue,abund_M0t)