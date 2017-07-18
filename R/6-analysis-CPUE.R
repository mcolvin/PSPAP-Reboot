# CPUE ANALYSIS:
## 1. TREND
### BIAS & PRECISION
## 2. ABUNDANCE
### BIAS & PRECISION
## 3. GET.TRND AND GET.ABUND TESTS

source("R/1_global.R")
source("R/2_functions.R")
source("R/3_load-and-clean.R")


# ## RUN COMMENTED IF ONLY RESULTS FROM A PARTICULAR REFERENCE
# ## POPULATION ARE DESIRED
# ### READ IN ALL AVAILABLE REFERENCE POPULATIONS
# pop_list<-dir("output", pattern="sim_pop_version_")
# 
# ### SELECT A REFERENCE POPULATION
# item<-2
# pop_ref<-strsplit(pop_list[item],"version_")[[1]][2]
# pop_ref<-strsplit(pop_ref, ".", fixed=TRUE)[[1]][1]
# 
# ### PULL THE CATCH DATA ASSOCIATED WITH THE REFERENCE POPULATION
# dat_files<-dir("output", pattern=paste0("catch_dat_",pop_ref))



## PULL CATCH DATA
dat_files<-dir("output", pattern="catch_dat_")


#############################
## 1. CPUE TREND ESTIMATES ##
#############################

# FIND THE TREND IN CPUE AND CHECK FOR HIGH CAPTURE PROBABILITIES
get_trnd<-lapply(dat_files, function(i)
  {
    # TREND
    dat<-readRDS(paste0("output/",i))
    out<-get.trnd(sim_dat=dat)
    # FLAGS
    samp<-dat$samp_dat
    samp$p<-samp$q*samp$f
    P<-aggregate(p~b_segment+bend_num+year+gear, samp, sum, subset=occasion==1)
    P$flag<-ifelse(P$p<0.4,0,ifelse(P$p<=1,1,2))  
    if(nrow(subset(P, flag!=0))==0) {out$flag=0}
    if(nrow(subset(P, flag!=0))!=0)
      {
        out<-merge(out,
                   aggregate(flag~gear, P, length, subset=flag!=0),
                   by="gear",all.x=TRUE)
        out$flag<-ifelse(is.na(out$flag),0,out$flag)
      }
    return(out)
  })
  
# PUT IN A PALLATABLE FORM AND ADD SIGNIFICANCE CHECK
get_trnd<-do.call(rbind, get_trnd)
get_trnd$sig<-ifelse(get_trnd$pval<0.05,1,0)
  #########################################################
  #### REMOVE RUNS WITH A CERTAIN NUMBER OF FLAGS HERE??? #
  #########################################################
  
# MAKE A SUMMARY TABLE OF RESULTS
df_trnd<-ddply(get_trnd, .(gear), summarize,
          mean_trnd=mean(trnd),
          mean_pval=mean(pval),
          mean_bias=mean(bias),
          mean_cv=mean(cv),
          mean_flags=mean(flag,na.rm=TRUE),
          power=sum(sig)/length(sig),
          max_pval=max(pval),
          max_bias=max(bias),
          min_bias=min(bias),
          max_cv=max(cv),
          max_flags=max(flag, na.rm=TRUE))

# STORE AND SAVE TREND INFORMATION
cpue_trnd<-list(trnd_dat=get_trnd, summary=df_trnd)
saveRDS(cpue_trnd,file=paste0("output/cpue_trnd_catchability_random_",
                              gsub(":", "_", Sys.time()),".rds"))

# ### SAVE FOR A PARTICULAR REFERENC POPULATION 
# saveRDS(cpue_trnd,file=paste0("output/cpue_trnd_",pop_ref,"_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))

proc.time()-ptm



#################################
## 2. CPUE ABUNDANCE ESTIMATES ##
#################################

# FIND CATCH BASED ABUNDANCE ESTIMATES
get_abund<-lapply(dat_files, function(i)
{
  # ABUNDANCE
  dat<-readRDS(paste0("output/",i))
  out<-get.abund(sim_dat=dat,bends=bends)
  return(out)
})
# PUT IN A PALLATABLE FORM AND ADD SIGNIFICANCE CHECK
get_abund<-do.call(rbind,get_abund)

# MAKE SUMMARY TABLE OF RESULTS
df_abund<-ddply(get_abund, .(b_segment,year,gear,abundance), summarize,
               mean_Nhat_AM=mean(Nhat_AM),
               sd_Nhat_AM=sd(Nhat_AM),
               mean_bias_AM=mean(bias_AM),
               mean_Nhat_HM=mean(Nhat_HM),
               sd_Nhat_HM=sd(Nhat_HM),
               mean_bias_HM=mean(bias_HM),
               sample_size=length(Nhat_HM))
df_abund$cv_AM<-df_abund$sd_Nhat_AM/df_abund$mean_Nhat_AM
df_abund$cv_HM<-df_abund$sd_Nhat_HM/df_abund$mean_Nhat_HM
  
# STORE AND SAVE ABUNDANCE INFORMATION
cpue_abund<-list(abund_dat=get_abund, summary=df_abund)
saveRDS(cpue_abund,file=paste0("output/cpue_abund_catchability_random_",
                              gsub(":", "_", Sys.time()),".rds"))

# ### SAVE FOR A PARTICULAR REFERENC POPULATION 
# saveRDS(cpue_trnd,file=paste0("output/cpue_abund_",pop_ref,"_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))

proc.time()-ptm






################################
##  TEST GET.TRND & GET>ABUND ##
################################
sim_dat<-readRDS("output/catch_dat_catchability_random_rep2017-07-07 11_43_38.rds")

get_trnd<-get.trnd(sim_dat)
get_trnd

get_abund<-get.abund(sim_dat,bends=bends)
head(get_abund)
