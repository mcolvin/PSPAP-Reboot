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
pop_list<-dir("output", pattern="sim_pop_version_")

### SELECT A REFERENCE POPULATION
item<-2
pop_ref<-strsplit(pop_list[item],"version_")[[1]][2]
pop_ref<-strsplit(pop_ref, ".", fixed=TRUE)[[1]][1]

### PULL THE CATCH DATA ASSOCIATED WITH THE REFERENCE POPULATION
dat_files<-dir("output", pattern=paste0("catch_dat_",pop_ref))


# ## RUN COMMENTED TO USE ALL AVAILABLE DATA 
# ## PULL CATCH DATA
# dat_files<-dir("_output", pattern="catch_dat_")


##########################
## 1. M0 & Mt ESTIMATES ##
##########################

# FIND M0 & Mt ESTIMATES FOR TREND AND ABUNDANCE
get_ests<-lapply(dat_files, function(i)
{
  # TREND & ABUNDANCE
  dat<-readRDS(paste0("_output/",i))
  out<-get.M0t.ests(sim_dat=dat,bends=bends)
  return(out)
})

# TREND
## PUT IN A PALLATABLE FORM
get_M0t_trnd<-do.call(rbind, sapply(get_ests, "[[", "M0t_trnd", simplify=FALSE))
get_M0t_trnd$sig_AM_M0<-ifelse(get_M0t_trnd$pval_AM_M0<0.05,1,0)
get_M0t_trnd$sig_WM_M0<-ifelse(get_M0t_trnd$pval_WM_M0<0.05,1,0)
get_M0t_trnd$sig_AM_Mt<-ifelse(get_M0t_trnd$pval_AM_Mt<0.05,1,0)
get_M0t_trnd$sig_WM_Mt<-ifelse(get_M0t_trnd$pval_WM_Mt<0.05,1,0)

## MAKE SUMMARY TABLE OF RESULTS
df_trnd<-ddply(get_M0t_trnd, .(gear), summarize,
               mean_trnd_AM_M0=mean(trnd_AM_M0, na.rm=TRUE),
               mean_pval_AM_M0=mean(pval_AM_M0, na.rm=TRUE),
               mean_bias_AM_M0=mean(bias_AM_M0, na.rm=TRUE),
               mean_cv_AM_M0=mean(cv_AM_M0, na.rm=TRUE),
               mean_cv_AM2_M0=mean(cv_AM_M0*trnd_AM_M0, na.rm=TRUE)/mean(trnd_AM_M0,na.rm=TRUE),
               power_AM_M0=sum(sig_AM_M0,na.rm=TRUE)/length(which(!is.na(sig_AM_M0))),
               mean_trnd_WM_M0=mean(trnd_WM_M0, na.rm=TRUE),
               mean_pval_WM_M0=mean(pval_WM_M0, na.rm=TRUE),
               mean_bias_WM_M0=mean(bias_WM_M0, na.rm=TRUE),
               mean_cv_WM_M0=mean(cv_WM_M0, na.rm=TRUE),
               mean_cv_WM2_M0=mean(cv_WM_M0*trnd_WM_M0, na.rm=TRUE)/mean(trnd_WM_M0,na.rm=TRUE),
               power_WM_M0=sum(sig_WM_M0,na.rm=TRUE)/length(which(!is.na(sig_WM_M0))),
               mean_performance_M0=mean(perform_M0),
               mean_trnd_AM_Mt=mean(trnd_AM_Mt, na.rm=TRUE),
               mean_pval_AM_Mt=mean(pval_AM_Mt, na.rm=TRUE),
               mean_bias_AM_Mt=mean(bias_AM_Mt, na.rm=TRUE),
               mean_cv_AM_Mt=mean(cv_AM_Mt, na.rm=TRUE),
               mean_cv_AM2_Mt=mean(cv_AM_Mt*trnd_AM_Mt, na.rm=TRUE)/mean(trnd_AM_Mt,na.rm=TRUE),
               power_AM_Mt=sum(sig_AM_Mt,na.rm=TRUE)/length(which(!is.na(sig_AM_Mt))),
               mean_trnd_WM_Mt=mean(trnd_WM_Mt, na.rm=TRUE),
               mean_pval_WM_Mt=mean(pval_WM_Mt, na.rm=TRUE),
               mean_bias_WM_Mt=mean(bias_WM_Mt, na.rm=TRUE),
               mean_cv_WM_Mt=mean(cv_WM_Mt, na.rm=TRUE),
               mean_cv_WM2_Mt=mean(cv_WM_Mt*trnd_WM_Mt, na.rm=TRUE)/mean(trnd_WM_Mt,na.rm=TRUE),
               power_WM_Mt=sum(sig_WM_Mt,na.rm=TRUE)/length(which(!is.na(sig_WM_Mt))),
               mean_performance_Mt=mean(perform_Mt))

# STORE AND SAVE TREND INFORMATION
M0t_trnd<-list(trnd_dat=get_M0t_trnd, summary=df_trnd, data=dat_files)
### SAVE FOR A PARTICULAR REFERENCE POPULATION
saveRDS(M0t_trnd,file=paste0("_output/M0t_trnd_",pop_ref,"_catchability_random_",
                              gsub(":", "_", Sys.time()),".rds"))
# ### SAVE FOR MULTIPLE REFERENCE POPULATIONS 
# saveRDS(M0t_trnd,file=paste0("_output/M0t_trnd_catchability_random_",
#                              gsub(":", "_", Sys.time()),".rds"))


# ABUNDANCE
## PUT IN A PALLATABLE FORM
get_M0t_abund<-do.call(rbind, sapply(get_ests, "[[", "M0t_abund", simplify=FALSE))

# MAKE SUMMARY TABLE OF RESULTS
df_abund<-ddply(get_M0t_abund, .(segment,year,gear), summarize,
                mean_Nhat_AM_M0=mean(Nhat_AM_M0, na.rm=TRUE),
                mean_bias_AM_M0=mean(abund_bias_AM_M0, na.rm=TRUE),
                mean_cv_AM_M0=mean(abund_cv_AM_M0, na.rm=TRUE),
                mean_cv_AM2_M0=mean(abund_cv_AM_M0*Nhat_AM_M0, na.rm=TRUE)/mean(Nhat_AM_M0,na.rm=TRUE),
                mean_Nhat_WM_M0=mean(Nhat_WM_M0, na.rm=TRUE),
                mean_bias_WM_M0=mean(abund_bias_WM_M0, na.rm=TRUE),
                mean_cv_WM_M0=mean(abund_cv_WM_M0, na.rm=TRUE),
                mean_cv_WM2_M0=mean(abund_cv_WM_M0*Nhat_WM_M0, na.rm=TRUE)/mean(Nhat_WM_M0,na.rm=TRUE),
                mean_perform_M0=mean(perform_M0, na.rm=TRUE),
                mean_Nhat_AM_Mt=mean(Nhat_AM_Mt, na.rm=TRUE),
                mean_bias_AM_Mt=mean(abund_bias_AM_Mt, na.rm=TRUE),
                mean_cv_AM_Mt=mean(abund_cv_AM_Mt, na.rm=TRUE),
                mean_cv_AM2_Mt=mean(abund_cv_AM_Mt*Nhat_AM_Mt, na.rm=TRUE)/mean(Nhat_AM_Mt,na.rm=TRUE),
                mean_Nhat_WM_Mt=mean(Nhat_WM_Mt, na.rm=TRUE),
                mean_bias_WM_Mt=mean(abund_bias_WM_Mt, na.rm=TRUE),
                mean_cv_WM_Mt=mean(abund_cv_WM_Mt, na.rm=TRUE),
                mean_cv_WM2_Mt=mean(abund_cv_WM_Mt*Nhat_WM_Mt, na.rm=TRUE)/mean(Nhat_WM_Mt,na.rm=TRUE),
                mean_perform_Mt=mean(perform_Mt, na.rm=TRUE))

# STORE AND SAVE ABUNDANCE INFORMATION
M0t_abund<-list(abund_dat=get_M0t_abund, summary=df_abund, data=dat_files)
### SAVE FOR A PARTICULAR REFERENC POPULATION
saveRDS(M0t_abund,file=paste0("_output/M0t_abund_",pop_ref,"_catchability_random_",
                              gsub(":", "_", Sys.time()),".rds"))
# ### SAVE FOR MULTIPLE REFERENCE POPULATIONS 
# saveRDS(M0t_abund,file=paste0("_output/M0t_abund_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))



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


