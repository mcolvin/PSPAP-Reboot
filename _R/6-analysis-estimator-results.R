if(pcname!="WF-FGNL842")
{
  catch_list<-dir("D:/_output/2-catch", pattern="catch_dat_")
  lapply(1:length(catch_list), function(x)
  {
    id<-strsplit(catch_list[x], "catch_dat_")[[1]][2]
    sim_dat<-readRDS(file=paste0("D:/_output/2-catch/", catch_list[x]))
    est_list<-dir("D:/_output/3-estimates", pattern=id)
    if(length(est_list)>0)
    {
      tmp<-lapply(1:length(est_list),function(y)
      {
        est<-readRDS(file=paste0("D:/_output/3-estimates/",est_list[y]))
        out<-abund.trnd(est=est,sim_dat=sim_dat)
        # REFORMAT M0t TREND OUTPUTS
        if(strsplit(est_list[y],"_est_")[[1]][1]=="M0t")
        {
          tmpA<-out$trnd[,c(1:6,11,12)]
          tmpA$estimator<-paste0(tmpA$estimator,"_AM")
          colnames(tmpA)<-gsub("_AM", "", colnames(tmpA))
          tmpW<-out$trnd[,c(1,2,7:12)]
          tmpW$estimator<-paste0(tmpW$estimator,"_WM")
          colnames(tmpW)<-gsub("_WM", "", colnames(tmpW))
          out$trnd<-rbind(tmpA,tmpW)
        }
        return(out)
      })
      # COLLECT OUTPUT
      trnd<-do.call(rbind,lapply(tmp, `[[`, 1))
      abund<-do.call(rbind,lapply(tmp, `[[`, 2))
      lgth<-length.dat(sim_dat = sim_dat)
      # SAVE OUTPUT
      out<-list(trnd=trnd, abund=abund, lgth=lgth)
      saveRDS(out,file=paste0("D:/_output/4-utilities/abund_trnd_lgth_",id))
      
      trnd$id<-strsplit(id,".rds")[[1]][1]
    }


lapply(1:length(est_list), function(x)
{
  id<-strsplit(est_list[x], "_est_")[[1]][2]
  if(pcname=="WF-FGNL842")
  {
    sim_dat<-readRDS(file=paste0("E:/_output/2-catch/catch_dat_", id))
    est<-readRDS(file=paste0("E:/_output/3-estimates/",est_list[x]))
  }
  if(pcname!="WF-FGNL842")
  {
    sim_dat<-readRDS(file=paste0("D:/_output/2-catch/catch_dat_", id))
    est<-readRDS(file=paste0("D:/_output/3-estimates/",est_list[x]))
  }
  out<-abund.trnd(est=est,sim_dat=sim_dat)
  out$length<-length.dat(sim_dat = sim_dat)
  out$extras<-data.frame(flags=, mean_q_input=, B0_sd_input=, mean_q_realized=, q_sd_realized=,samp_type=,deployments=,occasions=sim_dat$inputs$occasions,total_effort=,id=strsplit(id, ".rds"))
  if(pcname=="WF-FGNL842")
  {
    saveRDS(out,file=paste0("E:/_output/4-utilities/est_eval_", id))
  }
  if(pcname!="WF-FGNL842")
  {
    saveRDS(out,file=paste0("D:/_output/4-utilities/est_eval_", id))
  }
  
}


# ESTIMATOR RESULTS FOR CONDITIONAL PROBABILITY TABLES
## 1. TREND
## 2. ABUNDANCE

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

# TREND
## PULL CPUE TREND DATA
trnd_files<-dir("D:/DataDump", pattern=c("cpue_trnd_"))
### USE ONLY DATA WITH PARTICULAR SAMPLING STRATEGY
samp_type<-"r"
trnd_files<-trnd_files[grep(paste0("_samp_type_",samp_type),trnd_files)]
## CREATE DATAFRAME
trnd_cpue<-lapply(trnd_files, function(i)
{
  out<-readRDS(paste0("D:/DataDump/",i))
  out$perform<-1
  out$estimator<-"CPUE"
  out<-out[,c("gear","trnd", "bias", "cv", "flags", "perform", "estimator",
              "mean_q_input", "B0_sd_input","mean_q_realized", "q_sd_realized",
              "deployments","occasions","total_effort")]
  return(out)
})
trnd_cpue<-do.call("rbind",trnd_cpue)

## PULL M0t TREND DATA
trnd_files<-dir("D:/DataDump", pattern=c("M0t_trnd_"))
### USE ONLY DATA WITH PARTICULAR SAMPLING STRATEGY
samp_type<-"r"
trnd_files<-trnd_files[grep(paste0("_samp_type_",samp_type),trnd_files)]

## CREATE DATAFRAME
trnd_M0t<-lapply(trnd_files, function(i)
{
  out<-readRDS(paste0("D:/DataDump/",i))
  out<-out$trnd_dat
  out0A<-out[,c(1,3:5,21,11)]
  out0A$estimator<-"M0_AM"
  colnames(out0A)<-gsub("_AM_M0", "", colnames(out0A))
  out0W<-out[,c(1,7:9,22,11)]
  out0W$estimator<-"M0_WM"
  colnames(out0W)<-gsub("_WM_M0", "", colnames(out0W))
  outtA<-out[,c(1,12:14,23,20)]
  outtA$estimator<-"Mt_AM"
  colnames(outtA)<-gsub("_AM_Mt", "", colnames(outtA))
  outtW<-out[,c(1,16:18,24,20)]
  outtW$estimator<-"Mt_WM"
  colnames(outtW)<-gsub("_WM_Mt", "", colnames(outtW))
  out<-rbind(out0A, out0W,outtA, outtW)
  out<-out[,c("gear","trnd", "bias", "cv", "flags", "perform", "estimator",
              "mean_q_input", "B0_sd_input","mean_q_realized", "q_sd_realized",
              "deployments","occasions","total_effort")]
  return(out)
})
trnd_M0t<-do.call("rbind",trnd_M0t)

## COMBINE CPUE AND M0t TREND ANALYSIS
trnd_table<-rbind(trnd_cpue,trnd_M0t)


# ABUNDANCE
## PULL CPUE ABUNDANCE DATA
abund_files<-dir("D:/DataDump", pattern=c("cpue_abund_"))
# USE ONLY DATA WITH PARTICULAR SAMPLING STRATEGY
samp_type<-"r"
abund_files<-abund_files[grep(paste0("_samp_type_",samp_type),abund_files)]
# CREATE DATAFRAME
abund_cpue<-lapply(abund_files, function(i)
{
  # ABUNDANCE
  out<-readRDS(paste0("D:/DataDump/",i))
  out$perform<-1
  # ARITHMETIC MEAN
  outA<-out[,c(1:3,5,8,10,12:19,21)]
  colnames(outA)<-gsub("_AM", "", colnames(outA))
  outA$estimator<-"CPUE_AM"
  # WEIGHTED ARITHMETIC MEAN
  outW<-out[,c(1:3,6,9,11,12:19,21)]
  colnames(outW)<-gsub("_WM", "", colnames(outW))
  outW$estimator<-"CPUE_WM"
  out<-rbind(outA, outW)
  out<-out[,c("b_segment","year","gear","Nhat", "bias", "cv", "flags", "perform", "estimator",
              "mean_q_input", "B0_sd_input","mean_q_realized", "q_sd_realized",
              "deployments","occasions","total_effort")]
  return(out)
})
abund_cpue<-do.call("rbind",abund_cpue)


## PULL M0t ABUNDANCE DATA
abund_files<-dir("D:/DataDump", pattern=c("M0t_abund_"))
### USE ONLY DATA WITH PARTICULAR SAMPLING STRATEGY
samp_type<-"r"
abund_files<-abund_files[grep(paste0("_samp_type_",samp_type),abund_files)]

## CREATE DATAFRAME
abund_M0t<-lapply(abund_files, function(i)
{
  out<-readRDS(paste0("D:/DataDump/",i))
  out<-out$abund_dat
  out0A<-out[,c(1,3:5,21,11)]
  out0A$estimator<-"M0_AM"
  colnames(out0A)<-gsub("_AM_M0", "", colnames(out0A))
  out0W<-out[,c(1,7:9,22,11)]
  out0W$estimator<-"M0_WM"
  colnames(out0W)<-gsub("_WM_M0", "", colnames(out0W))
  outtA<-out[,c(1,12:14,23,20)]
  outtA$estimator<-"Mt_AM"
  colnames(outtA)<-gsub("_AM_Mt", "", colnames(outtA))
  outtW<-out[,c(1,16:18,24,20)]
  outtW$estimator<-"Mt_WM"
  colnames(outtW)<-gsub("_WM_Mt", "", colnames(outtW))
  out<-rbind(out0A, out0W,outtA, outtW)
  out<-out[,c("gear","Nhat", "bias", "cv", "flags", "perform", "estimator",
              "mean_q_input", "B0_sd_input","mean_q_realized", "q_sd_realized",
              "deployments","occasions","total_effort")]
  return(out)
})
abund_M0t<-do.call("rbind",abund_M0t)

## COMBINE CPUE AND M0t TREND ANALYSIS
abund_table<-rbind(abund_cpue,abund_M0t)