# M0 & Mt ANALYSIS:
## 1. M0 & Mt ESTIMATES: TREND & ABUNDANCE
### BIAS & PRECISION
## 2. GET.M0t.ESTS FUNCTION TEST

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

## RUN COMMENTED IF ONLY RESULTS FROM A PARTICULAR REFERENCE
## POPULATION ARE DESIRED
### READ IN ALL AVAILABLE REFERENCE POPULATIONS
#pop_list<-dir("_output", pattern="sim_pop_version_")
pop_list<-dir("E:/_output", pattern="sim_pop_version_")

### SELECT A REFERENCE POPULATION
item<-1
pop_ref<-strsplit(pop_list[item],"version_")[[1]][2]
pop_ref<-strsplit(pop_ref, ".", fixed=TRUE)[[1]][1]
samp_type<-"r"
#samp_type<-"f"

### PULL THE CATCH DATA ASSOCIATED WITH THE REFERENCE POPULATION
# dat_files<-dir("_output", pattern=paste0("catch_dat_",pop_ref,
#                                         "_samp_type_",samp_type))
dat_files<-dir("E:/_output", pattern=paste0("catch_dat_",pop_ref,
                                        "_samp_type_",samp_type))

# ## RUN COMMENTED TO USE ALL AVAILABLE DATA 
# ## PULL CATCH DATA
# dat_files<-dir("_output", pattern="catch_dat_")


##########################
## 1. M0 & Mt ESTIMATES ##
##########################

# FIND M0 & Mt ESTIMATES FOR TREND AND ABUNDANCE
ptm<-proc.time()
get_ests<-lapply(dat_files, function(i)
{
  # TREND & ABUNDANCE
  #dat<-readRDS(paste0("_output/",i))
  dat<-readRDS(paste0("E:/_output/",i))
  out<-get.M0t.ests(sim_dat=dat,bends=bends)
  colnames(out$M0t_abund)[which(colnames(out$M0t_abund)=="segment")]<-"b_segment"
  # FLAGS
  samp<-dat$samp_dat
  samp$p<-samp$q*samp$f
  P<-aggregate(p~b_segment+bend_num+year+gear, samp, sum, subset=occasion==1)
  P$flag<-ifelse(P$p<0.4,0,ifelse(P$p<=1,1,2)) 
  P_trnd<-ddply(P, .(gear), summarize,
                flags=length(which(flag!=0))/length(flag))
  out$M0t_trnd<-merge(out$M0t_trnd,P_trnd, by="gear")
  q_stats_trnd<-ddply(subset(samp, occasion==1), .(gear),
                      summarize,
                      mean_q_input=mean(q_mean),
                      B0_sd_input=mean(B0_sd),
                      mean_q_realized=mean(q),
                      q_sd_realized=sd(q))
  out$M0t_trnd<-merge(out$M0t_trnd, q_stats_trnd, by="gear")
  out$M0t_trnd$deployments<-length(unique(samp$deployment))
  out$M0t_trnd$occasions<-length(unique(samp$occasion))
  out$M0t_trnd$total_effort<-sum(samp$f)
  out$M0t_trnd$id<-strsplit(strsplit(i,"rep_")[[1]][2], ".", fixed=TRUE)[[1]][1]
  P_abund<-ddply(P, .(b_segment, year, gear), summarize,
                 flags=length(which(flag!=0))/length(flag))
  out$M0t_abund<-merge(out$M0t_abund,P_abund,by=c("b_segment","year","gear"))
  q_stats_abund<-ddply(subset(samp, occasion==1), .(b_segment, year, gear),
                       summarize,
                       mean_q_input=mean(q_mean),
                       B0_sd_input=mean(B0_sd),
                       mean_q_realized=mean(q),
                       q_sd_realized=sd(q))
  out$M0t_abund<-merge(out$M0t_abund,q_stats_abund,by=c("b_segment","year","gear"))
  out$M0t_abund$deployments<-length(unique(samp$deployment))
  out$M0t_abund$occasions<-length(unique(samp$occasion))
  out$M0t_abund$total_effort<-sum(samp$f)
  out$M0t_abund$id<-strsplit(strsplit(i,"rep_")[[1]][2], ".", fixed=TRUE)[[1]][1]
  saveRDS(out,file=paste0("E:/_output/M0t_trnd_",pop_ref,"_samp_type_",samp_type,
                               "_catchability_random_mini_output",
                               gsub(":", "_", Sys.time()),".rds"))
  return(out)
})
proc.time()-ptm

# TREND
## PUT IN A PALLATABLE FORM
get_M0t_trnd<-do.call(rbind, sapply(get_ests, "[[", "M0t_trnd", simplify=FALSE))
# get_M0t_trnd$sig_AM_M0<-ifelse(get_M0t_trnd$pval_AM_M0<0.05,1,0)
# get_M0t_trnd$sig_WM_M0<-ifelse(get_M0t_trnd$pval_WM_M0<0.05,1,0)
# get_M0t_trnd$sig_AM_Mt<-ifelse(get_M0t_trnd$pval_AM_Mt<0.05,1,0)
# get_M0t_trnd$sig_WM_Mt<-ifelse(get_M0t_trnd$pval_WM_Mt<0.05,1,0)

# ## MAKE SUMMARY TABLE OF RESULTS
# df_trnd<-ddply(get_M0t_trnd, .(gear), summarize,
#                mean_trnd_AM_M0=mean(trnd_AM_M0, na.rm=TRUE),
#                mean_pval_AM_M0=mean(pval_AM_M0, na.rm=TRUE),
#                mean_bias_AM_M0=mean(bias_AM_M0, na.rm=TRUE),
#                mean_cv_AM_M0=mean(cv_AM_M0, na.rm=TRUE),
#                mean_cv_AM2_M0=mean(cv_AM_M0*trnd_AM_M0, na.rm=TRUE)/mean(trnd_AM_M0,na.rm=TRUE),
#                power_AM_M0=sum(sig_AM_M0,na.rm=TRUE)/length(which(!is.na(sig_AM_M0))),
#                mean_trnd_WM_M0=mean(trnd_WM_M0, na.rm=TRUE),
#                mean_pval_WM_M0=mean(pval_WM_M0, na.rm=TRUE),
#                mean_bias_WM_M0=mean(bias_WM_M0, na.rm=TRUE),
#                mean_cv_WM_M0=mean(cv_WM_M0, na.rm=TRUE),
#                mean_cv_WM2_M0=mean(cv_WM_M0*trnd_WM_M0, na.rm=TRUE)/mean(trnd_WM_M0,na.rm=TRUE),
#                power_WM_M0=sum(sig_WM_M0,na.rm=TRUE)/length(which(!is.na(sig_WM_M0))),
#                mean_performance_M0=mean(perform_M0),
#                mean_trnd_AM_Mt=mean(trnd_AM_Mt, na.rm=TRUE),
#                mean_pval_AM_Mt=mean(pval_AM_Mt, na.rm=TRUE),
#                mean_bias_AM_Mt=mean(bias_AM_Mt, na.rm=TRUE),
#                mean_cv_AM_Mt=mean(cv_AM_Mt, na.rm=TRUE),
#                mean_cv_AM2_Mt=mean(cv_AM_Mt*trnd_AM_Mt, na.rm=TRUE)/mean(trnd_AM_Mt,na.rm=TRUE),
#                power_AM_Mt=sum(sig_AM_Mt,na.rm=TRUE)/length(which(!is.na(sig_AM_Mt))),
#                mean_trnd_WM_Mt=mean(trnd_WM_Mt, na.rm=TRUE),
#                mean_pval_WM_Mt=mean(pval_WM_Mt, na.rm=TRUE),
#                mean_bias_WM_Mt=mean(bias_WM_Mt, na.rm=TRUE),
#                mean_cv_WM_Mt=mean(cv_WM_Mt, na.rm=TRUE),
#                mean_cv_WM2_Mt=mean(cv_WM_Mt*trnd_WM_Mt, na.rm=TRUE)/mean(trnd_WM_Mt,na.rm=TRUE),
#                power_WM_Mt=sum(sig_WM_Mt,na.rm=TRUE)/length(which(!is.na(sig_WM_Mt))),
#                mean_performance_Mt=mean(perform_Mt))

# STORE AND SAVE TREND INFORMATION
M0t_trnd<-get_M0t_trnd#list(trnd_dat=get_M0t_trnd, summary=df_trnd)
### SAVE FOR A PARTICULAR REFERENCE POPULATION
# saveRDS(M0t_trnd,file=paste0("_output/M0t_trnd_",pop_ref,"_samp_type_",samp_type,
#                              "_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))
saveRDS(M0t_trnd,file=paste0("E:/_output/M0t_trnd_",pop_ref,"_samp_type_",samp_type,
                             "_catchability_random_",
                             gsub(":", "_", Sys.time()),".rds"))
# ### SAVE FOR MULTIPLE REFERENCE POPULATIONS 
# saveRDS(M0t_trnd,file=paste0("_output/M0t_trnd_samp_type_",samp_type,"_catchability_random_",
#                              gsub(":", "_", Sys.time()),".rds"))


# ABUNDANCE
## PUT IN A PALLATABLE FORM
get_M0t_abund<-do.call(rbind, sapply(get_ests, "[[", "M0t_abund", simplify=FALSE))

# # MAKE SUMMARY TABLE OF RESULTS
# df_abund<-ddply(get_M0t_abund, .(segment,year,gear), summarize,
#                 mean_Nhat_AM_M0=mean(Nhat_AM_M0, na.rm=TRUE),
#                 mean_bias_AM_M0=mean(abund_bias_AM_M0, na.rm=TRUE),
#                 mean_cv_AM_M0=mean(abund_cv_AM_M0, na.rm=TRUE),
#                 mean_cv_AM2_M0=mean(abund_cv_AM_M0*Nhat_AM_M0, na.rm=TRUE)/mean(Nhat_AM_M0,na.rm=TRUE),
#                 mean_Nhat_WM_M0=mean(Nhat_WM_M0, na.rm=TRUE),
#                 mean_bias_WM_M0=mean(abund_bias_WM_M0, na.rm=TRUE),
#                 mean_cv_WM_M0=mean(abund_cv_WM_M0, na.rm=TRUE),
#                 mean_cv_WM2_M0=mean(abund_cv_WM_M0*Nhat_WM_M0, na.rm=TRUE)/mean(Nhat_WM_M0,na.rm=TRUE),
#                 mean_perform_M0=mean(perform_M0, na.rm=TRUE),
#                 mean_Nhat_AM_Mt=mean(Nhat_AM_Mt, na.rm=TRUE),
#                 mean_bias_AM_Mt=mean(abund_bias_AM_Mt, na.rm=TRUE),
#                 mean_cv_AM_Mt=mean(abund_cv_AM_Mt, na.rm=TRUE),
#                 mean_cv_AM2_Mt=mean(abund_cv_AM_Mt*Nhat_AM_Mt, na.rm=TRUE)/mean(Nhat_AM_Mt,na.rm=TRUE),
#                 mean_Nhat_WM_Mt=mean(Nhat_WM_Mt, na.rm=TRUE),
#                 mean_bias_WM_Mt=mean(abund_bias_WM_Mt, na.rm=TRUE),
#                 mean_cv_WM_Mt=mean(abund_cv_WM_Mt, na.rm=TRUE),
#                 mean_cv_WM2_Mt=mean(abund_cv_WM_Mt*Nhat_WM_Mt, na.rm=TRUE)/mean(Nhat_WM_Mt,na.rm=TRUE),
#                 mean_perform_Mt=mean(perform_Mt, na.rm=TRUE))

# STORE AND SAVE ABUNDANCE INFORMATION
M0t_abund<-get_M0t_abund#list(abund_dat=get_M0t_abund, summary=df_abund)
### SAVE FOR A PARTICULAR REFERENC POPULATION
# saveRDS(M0t_abund,file=paste0("_output/M0t_abund_",pop_ref,"_samp_type_",
#                               samp_type,"_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))
saveRDS(M0t_abund,file=paste0("E:/_output/M0t_abund_",pop_ref,"_samp_type_",
                              samp_type,"_catchability_random_",
                              gsub(":", "_", Sys.time()),".rds"))
# ### SAVE FOR MULTIPLE REFERENCE POPULATIONS 
# saveRDS(M0t_abund,file=paste0("_output/M0t_abund_samp_type_",samp_type,"_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))
proc.time()-ptm


###########################
##  2. TEST GET.M0t.ESTS  ##
###########################

## PULL ONE REPLICATE OF CATCH DATA
dat_files<-dir("output", pattern="catch_dat_")
sim_dat<-readRDS(paste0("output/",dat_files[2]))

## TEST FUNCTION
ptm<-proc.time()
yy<-get.M0t.ests(sim_dat=sim_dat, bends=bends)
proc.time()-ptm
# user      system    elapsed 
# 817.50    36.12     864.53 


