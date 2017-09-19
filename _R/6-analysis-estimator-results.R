
source("_R/1_global.r")

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
    atl<-abund.trnd(samp_type=samp_type, pop_num=i, catch_num=j,location=loc)
    sim_dat<-readRDS(file=paste0(loc,"_output/2-catch/catch_dat_",samp_type,"_",i,"-",j,".rds"))
    atl$lgth<-length.dat(sim_dat = sim_dat)
    write.csv(atl$trnd, file=paste0(loc,"_output/4-utilities/trnd_mini_",samp_type,"_",i,"-",j,".csv"),row.names=FALSE)
    write.csv(atl$abund, file=paste0(loc,"_output/4-utilities/abund_mini_",samp_type,"_",i,"-",j,".csv"),row.names=FALSE)
    write.csv(atl$trnd, file=paste0(loc,"_output/4-utilities/lgth_mini_",samp_type,"_",i,"-",j,".csv"),row.names=FALSE)
    return(atl)
  })
  ### CLOSE CLUSTERS
  stopCluster(cl)
  trnd<-do.call(rbind,lapply(outj, `[[`, 1))
  abund<-do.call(rbind,lapply(outj, `[[`, 2))
  lgth<-do.call(rbind,lapply(outj, `[[`, 3))
  return(list(trnd=trnd, abund=abund, lgth=lgth))
})

# SAVE COMPILED TABLES & MOVE MINIS TO COMPILED FOLDER
## COMPILE NEW RESULTS
trnd<-do.call(rbind,lapply(outi, `[[`, 1))
abund<-do.call(rbind,lapply(outi, `[[`, 2))
lgth<-do.call(rbind,lapply(outi, `[[`, 3))
## ADD TO MASTER TABLE
### IF PREVIOUS TABLE EXISTS
#### TREND
if(file.exists(paste0(loc, "_output/4-utilities/trnd_table.csv")))
{
  trnd_table<-read.csv(file=paste0(loc, "_output/4-utilities/trnd_table.csv"))
  trnd_table<-rbind(trnd_table,trnd)
  write.csv(trnd_table,file=paste0(loc, "_output/4-utilities/trnd_table.csv"),row.names=FALSE)
}
#### ABUNDANCE
if(file.exists(paste0(loc, "_output/4-utilities/abund_table.csv")))
{
  abund_table<-read.csv(file=paste0(loc, "_output/4-utilities/abund_table.csv"))
  abund_table<-rbind(abund_table,abund)
  write.csv(abund_table,file=paste0(loc, "_output/4-utilities/abund_table.csv"),row.names=FALSE)
}
#### LENGTH
if(file.exists(paste0(loc, "_output/4-utilities/lgth_table.csv")))
{
  lgth_table<-read.csv(file=paste0(loc, "_output/4-utilities/lgth_table.csv"))
  lgth_table<-rbind(lgth_table,lgth)
  write.csv(lgth_table,file=paste0(loc, "_output/4-utilities/lgth_table.csv"),row.names=FALSE)
}
### IF NO PREVIOUS TABLE EXISTS
#### TREND
if(!file.exists(paste0(loc, "_output/4-utilities/trnd_table.csv")))
{
  write.csv(trnd,file=paste0(loc, "_output/4-utilities/trnd_table.csv"),row.names=FALSE)
}
#### ABUNDANCE
if(!file.exists(paste0(loc, "_output/4-utilities/abund_table.csv")))
{
  write.csv(abund,file=paste0(loc, "_output/4-utilities/abund_table.csv"),row.names=FALSE)
}
#### LENGTH
if(!file.exists(paste0(loc, "_output/4-utilities/lgth_table.csv")))
{
  write.csv(lgth,file=paste0(loc, "_output/4-utilities/lgth_table.csv"),row.names=FALSE)
}
## MOVE NEW MINIS TO COMPILED FOLDER
new_minis<-dir(paste0(loc, "_output/4-utilities"), pattern="_mini_")
lapply(new_minis, function(x)
{
  file.rename(from=paste0(loc, "_output/4-utilities/", x), 
              to=paste0(loc, "_output/4-utilities/compiled_minis/", x))
})




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