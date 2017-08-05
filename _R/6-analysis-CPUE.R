# CPUE ANALYSIS:
## 1. TREND
### BIAS & PRECISION
## 2. ABUNDANCE
### BIAS & PRECISION
## 3. GET.TRND AND GET.ABUND TESTS

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")


## RUN IF ONLY RESULTS FROM A PARTICULAR REFERENCE
## POPULATION ARE DESIRED
### READ IN ALL AVAILABLE REFERENCE POPULATIONS
pop_list<-dir("output", pattern="sim_pop_version_")

### SELECT A REFERENCE POPULATION
item<-2
pop_ref<-strsplit(pop_list[item],"version_")[[1]][2]
pop_ref<-strsplit(pop_ref, ".", fixed=TRUE)[[1]][1]
samp_type<-"r"
#samp_type<-"f"

### PULL THE CATCH DATA ASSOCIATED WITH THE REFERENCE POPULATION
dat_files<-dir("output", pattern=paste0("catch_dat_",pop_ref,
                                        "_samp_type_",samp_type))


# ## RUN TO USE ALL AVAILABLE DATA 
# ## PULL CATCH DATA
# dat_files<-dir("output", pattern="catch_dat_")


#############################
## 1. CPUE TREND ESTIMATES ##
#############################

# FIND THE TREND IN CPUE AND CHECK FOR HIGH CAPTURE PROBABILITIES
get_trnd<-lapply(dat_files, function(i)
  {
    # TREND
    dat<-readRDS(paste0("C:/Users/sreynolds/Desktop/DataDump/output/",i))
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
  
# MAKE A SUMMARY TABLE OF RESULTS
df_trnd<-ddply(get_trnd, .(gear), summarize,
          mean_trnd=mean(trnd),
          mean_pval=mean(pval),
          mean_bias=mean(bias),
          mean_cv=mean(cv),
          mean_cv2=mean(cv*abs(trnd))/abs(mean(trnd)),
          mean_flags=mean(flag,na.rm=TRUE),
          power=sum(sig)/length(sig))

# STORE AND SAVE TREND INFORMATION
cpue_trnd<-list(trnd_dat=get_trnd, summary=df_trnd, data=dat_files)
### SAVE FOR A PARTICULAR REFERENCE POPULATION
saveRDS(cpue_trnd,file=paste0("_output/cpue_trnd_",pop_ref,"_samp_type_",
                              samp_type,"_catchability_random_",
                              gsub(":", "_", Sys.time()),".rds"))
# ### SAVE FOR MULTIPLE REFERENCE POPULATIONS 
# saveRDS(cpue_trnd,file=paste0("_output/cpue_trnd_samp_type_",
#                               samp_type,"_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))


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
# PUT IN A PALLATABLE FORM
get_abund<-do.call(rbind,get_abund)

# MAKE SUMMARY TABLE OF RESULTS
df_abund<-ddply(get_abund, .(b_segment,year,gear), summarize,
               mean_Nhat_AM=mean(Nhat_AM),
               mean_bias_AM=mean(bias_AM),
               mean_cv_AM=mean(cv_AM),
               mean_cv_AM2=mean(cv_AM*Nhat_AM)/mean(Nhat_AM),
               mean_Nhat_WM=mean(Nhat_WM),
               mean_bias_WM=mean(bias_WM),
               mean_cv_WM=mean(cv_WM),
               mean_cv_WM2=mean(cv_WM*Nhat_WM)/mean(Nhat_WM))
  
# STORE AND SAVE ABUNDANCE INFORMATION
cpue_abund<-list(abund_dat=get_abund, summary=df_abund, data=dat_files)
### SAVE FOR A PARTICULAR REFERENCE POPULATION
saveRDS(cpue_abund,file=paste0("_output/cpue_abund_",pop_ref,"_samp_type_",
                               samp_type,"_catchability_random_",
                               gsub(":", "_", Sys.time()),".rds"))
# ### SAVE FOR MULTIPLE REFERENCE POPULATIONS 
# saveRDS(cpue_abund,file=paste0("_output/cpue_abund_samp_type_",samp_type,
#                                "_catchability_random_",
#                                gsub(":", "_", Sys.time()),".rds"))




################################
##  TEST GET.TRND & GET.ABUND ##
################################
sim_dat<-readRDS("output/catch_dat_catchability_random_rep2017-07-07 11_43_38.rds")

get_trnd<-get.trnd(sim_dat)
get_trnd

get_abund<-get.abund(sim_dat,bends=bends)
head(get_abund)
