# MINIMUM KNOWN ALIVE (MKA) ESTIMATES:
## 1. CATCH, EFFORT, AND MKA BY BEND, YEAR, AND GEAR
## 2. TESTS

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")


######################
## 1. MKA ESTIMATES ##
######################
ptm<-proc.time()
repeats<-lapply(1:400, function(i)
{
  if(pcname=="WF-FGNL842")
  {
    catch_list<-dir("E:/_output/2-catch", pattern=paste0("catch_dat_r_",i,"-"))
    catch_list<-c(catch_list,dir("E:/_output/2-catch", 
                                 pattern=paste0("catch_dat_f_",i,"-")))
  }
  if(pcname!="WF-FGNL842")
  {
    catch_list<-dir("_output/2-catch", pattern=paste0("catch_dat_r_",i,"-"))
    catch_list<-c(catch_list,dir("_output/2-catch", 
                                 pattern=paste0("catch_dat_f_",i,"-")))
  }
  library(parallel)
  ## USE ALL CORES
  numCores<-detectCores()
  ## INITIATE CLUSTER
  cl<-makeCluster(numCores)
  ## MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
  clusterExport(cl, c("pcname","catch_list"),envir=environment())
  clusterEvalQ(cl, source("_R/2_functions.R"))
  clusterEvalQ(cl, library(plyr))
  clusterEvalQ(cl, library(reshape2))
  out<-parLapply(cl,1:length(catch_list), function(j)
  {
    # READ IN DATA
    if(pcname=="WF-FGNL842")
      {sim_dat<-readRDS(file=paste0("E:/_output/2-catch/",catch_list[j]))}
    if(pcname!="WF-FGNL842")
      {sim_dat<-readRDS(file=paste0("_output/2-catch/",catch_list[j]))}
    # SET OCCASIONS TO BE USED
    occasions<-2:4
    lapply(occasions, function(y)
    {
      # GET MKA ESTIMATES
      est<-MKA.ests(sim_dat=sim_dat, max_occ = y)
      # SAVE ESTIMATES
      if(pcname=="WF-FGNL842")
        {
          if(file.exists(paste0("E:/_output/3-estimates/MKA_est", 
                                strsplit(catch_list[j], "catch_dat")[[1]][2])))
            {
              old<-readRDS(file=paste0("E:/_output/3-estimates/MKA_est", 
                                       strsplit(catch_list[j], "catch_dat")[[1]][2]))
              est<-rbind(old,est)
          }
          saveRDS(est,
               file=paste0("E:/_output/3-estimates/MKA_est", 
                           strsplit(catch_list[j], "catch_dat")[[1]][2]))
        }
      if(pcname!="WF-FGNL842")
        {
        if(file.exists(paste0("_output/3-estimates/MKA_est", 
                                strsplit(catch_list[j], "catch_dat")[[1]][2])))
            {
              old<-readRDS(file=paste0("_output/3-estimates/MKA_est", 
                                       strsplit(catch_list[j], "catch_dat")[[1]][2]))
              est<-rbind(old,est)
            }
          saveRDS(est,
                  file=paste0("_output/3-estimates/MKA_est", 
                              strsplit(catch_list[j], "catch_dat")[[1]][2]))
        }
    })
    if(pcname=="WF-FGNL842")
    {
      est<-readRDS(file=paste0("E:/_output/3-estimates/MKA_est", 
                               strsplit(catch_list[j], "catch_dat")[[1]][2]))
    }
    if(pcname!="WF-FGNL842")
    {
      est<-readRDS(file=paste0("_output/3-estimates/MKA_est", 
                               strsplit(catch_list[j], "catch_dat")[[1]][2]))
    }
    out<-NULL
    if(anyDuplicated(est)>0){out<-strsplit(catch_list[j], "catch_dat")[[1]][2]}
    return(out)
  })
  stopCluster(cl)
  out<-do.call("rbind",out)
  return(out)
})
proc.time()-ptm

repeats<-do.call("rbind", repeats)
length(repeats)

#est<-readRDS("E:/_output/3-estimates/MKA_est_r_0-2.rds")
#head(est)
#tail(est)






















# ## RUN IF ONLY RESULTS FROM A PARTICULAR REFERENCE
# ## POPULATION ARE DESIRED
# ### READ IN ALL AVAILABLE REFERENCE POPULATIONS
# #pop_list<-dir("_output", pattern="sim_pop_version_")
# pop_list<-dir("D:/DataDump", pattern="sim_pop_version_")
# 
# ### SELECT A REFERENCE POPULATION
# item<-6
# pop_ref<-strsplit(pop_list[item],"version_")[[1]][2]
# pop_ref<-strsplit(pop_ref, ".", fixed=TRUE)[[1]][1]
# samp_type<-"r"
# #samp_type<-"f"
# 
# ### PULL THE CATCH DATA ASSOCIATED WITH THE REFERENCE POPULATION
# # dat_files<-dir("output", pattern=paste0("catch_dat_",pop_ref,
# #                                         "_samp_type_",samp_type))
# dat_files<-dir("D:/DataDump", pattern=paste0("catch_dat_",pop_ref,
#                                         "_samp_type_",samp_type))
# 
# # ## RUN TO USE ALL AVAILABLE DATA 
# # ## PULL CATCH DATA
# # dat_files<-dir("output", pattern="catch_dat_")
# 
# 
# 
# ptm<-proc.time()
# # FIND THE TREND IN CPUE AND CHECK FOR HIGH CAPTURE PROBABILITIES
# ## RUN IN PARALLEL
# library(parallel)
# ### USE ALL CORES
# numCores<-detectCores()
# ### INITIATE CLUSTER
# cl<-makeCluster(numCores)
# clusterEvalQ(cl, source("_R/2_functions.R"))
# clusterEvalQ(cl, library(plyr))
# ### RUN
# get_trnd<-parLapply(cl,dat_files, function(i)
#   {
#     # TREND
#     dat<-readRDS(paste0("D:/DataDump/",i))
#     out<-get.trnd(sim_dat=dat)
#     # FLAGS
#     samp<-dat$samp_dat
#     samp$p<-samp$q*samp$f
#     P<-aggregate(p~b_segment+bend_num+year+gear, samp, sum, subset=occasion==1)
#     P$flag<-ifelse(P$p<0.4,0,ifelse(P$p<=1,1,2))  
#     P<-ddply(P, .(gear), summarize,
#              flags=length(which(flag!=0))/length(flag))
#     out<-merge(out,P)
#     # CATCHABILITY
#     q_stats<-ddply(subset(samp, occasion==1), .(gear),
#                    summarize,
#                    mean_q_input=mean(q_mean),
#                    B0_sd_input=mean(B0_sd),
#                    mean_q_realized=mean(q),
#                    q_sd_realized=sd(q))
#     out<-merge(out, q_stats, by="gear")
#     # DEPLOYMENTS, OCCASIONS, TOTAL EFFORT, & DATA ID
#     out$deployments<-length(unique(samp$deployment))
#     out$occasions<-1
#     out$total_effort<-sum(samp$f)
#     out$id<-strsplit(strsplit(i,"rep_")[[1]][2], ".", fixed=TRUE)[[1]][1]
#     return(out)
#   })
# stopCluster(cl)
#   
# # PUT IN A PALLATABLE FORM AND ADD SIGNIFICANCE CHECK
# get_trnd<-do.call(rbind, get_trnd)
# get_trnd$sig<-ifelse(get_trnd$pval<0.05,1,0)
#   
# # # MAKE A SUMMARY TABLE OF RESULTS
# # df_trnd<-ddply(get_trnd, .(gear), summarize,
# #           mean_trnd=mean(trnd),
# #           mean_pval=mean(pval),
# #           mean_bias=mean(bias),
# #           mean_cv=mean(cv),
# #           mean_cv2=mean(cv*abs(trnd))/abs(mean(trnd)),
# #           mean_flags=mean(flag,na.rm=TRUE),
# #           power=sum(sig)/length(sig))
# 
# # STORE AND SAVE TREND INFORMATION
# cpue_trnd<-get_trnd#list(trnd_dat=get_trnd, summary=df_trnd)
# ### SAVE FOR A PARTICULAR REFERENCE POPULATION
# # saveRDS(cpue_trnd,file=paste0("_output/cpue_trnd_",pop_ref,"_samp_type_",
# #                               samp_type,"_catchability_random_",
# #                               gsub(":", "_", Sys.time()),".rds"))
# saveRDS(cpue_trnd,file=paste0("D:/DataDump/cpue_trnd_",pop_ref,"_samp_type_",
#                               samp_type,"_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))
# # ### SAVE FOR MULTIPLE REFERENCE POPULATIONS 
# # saveRDS(cpue_trnd,file=paste0("_output/cpue_trnd_samp_type_",
# #                               samp_type,"_catchability_random_",
# #                               gsub(":", "_", Sys.time()),".rds"))
# proc.time()-ptm
# #user     system    elapsed 
# #0.08     0.02      104.71
# 
# #################################
# ## 2. CPUE ABUNDANCE ESTIMATES ##
# #################################
# ptm<-proc.time()
# # FIND CATCH BASED ABUNDANCE ESTIMATES
# ## RUN IN PARALLEL
# library(parallel)
# ### USE ALL CORES
# numCores<-detectCores()
# ### INITIATE CLUSTER
# cl<-makeCluster(numCores)
# clusterExport(cl,"bends")
# clusterEvalQ(cl, source("_R/2_functions.R"))
# clusterEvalQ(cl, library(plyr))
# ### RUN
# get_abund<-parLapply(cl,dat_files, function(i)
# {
#   # ABUNDANCE
#   #dat<-readRDS(paste0("_output/",i))
#   dat<-readRDS(paste0("D:/DataDump/",i))
#   out<-get.abund(sim_dat=dat,bends=bends)
#   # FLAGS
#   samp<-dat$samp_dat
#   samp$p<-samp$q*samp$f
#   P<-aggregate(p~b_segment+bend_num+year+gear, samp, sum, subset=occasion==1)
#   P$flag<-ifelse(P$p<0.4,0,ifelse(P$p<=1,1,2))  
#   P<-ddply(P, .(b_segment, year, gear), summarize,
#            flags=length(which(flag!=0))/length(flag))
#   out<-merge(out,P, by=c("b_segment", "year", "gear"))
#   # CATCHABILITY
#   q_stats<-ddply(subset(samp, occasion==1), .(b_segment, year, gear),
#                  summarize,
#                  mean_q_input=mean(q_mean),
#                  B0_sd_input=mean(B0_sd),
#                  mean_q_realized=mean(q),
#                  q_sd_realized=sd(q))
#   out<-merge(out, q_stats, by=c("b_segment", "year", "gear"))
#   # DEPLOYMENTS, OCCASIONS, TOTAL EFFORT, & DATA ID
#   out$deployments<-length(unique(samp$deployment))
#   out$occasions<-1
#   out$total_effort<-sum(samp$f)
#   out$id<-strsplit(strsplit(i,"rep_")[[1]][2], ".", fixed=TRUE)[[1]][1]
#   return(out)
# })
# stopCluster(cl)
# 
# # PUT IN A PALLATABLE FORM
# get_abund<-do.call(rbind,get_abund)
# 
# # # MAKE SUMMARY TABLE OF RESULTS
# # df_abund<-ddply(get_abund, .(b_segment,year,gear), summarize,
# #                mean_Nhat_AM=mean(Nhat_AM),
# #                mean_bias_AM=mean(bias_AM),
# #                mean_cv_AM=mean(cv_AM),
# #                mean_cv_AM2=mean(cv_AM*Nhat_AM)/mean(Nhat_AM),
# #                mean_Nhat_WM=mean(Nhat_WM),
# #                mean_bias_WM=mean(bias_WM),
# #                mean_cv_WM=mean(cv_WM),
# #                mean_cv_WM2=mean(cv_WM*Nhat_WM)/mean(Nhat_WM))
#   
# # STORE AND SAVE ABUNDANCE INFORMATION
# cpue_abund<-get_abund#list(abund_dat=get_abund, summary=df_abund)
# ### SAVE FOR A PARTICULAR REFERENCE POPULATION
# # saveRDS(cpue_abund,file=paste0("_output/cpue_abund_",pop_ref,"_samp_type_",
# #                                samp_type,"_catchability_random_",
# #                                gsub(":", "_", Sys.time()),".rds"))
# saveRDS(cpue_abund,file=paste0("D:/DataDump/cpue_abund_",pop_ref,"_samp_type_",
#                                samp_type,"_catchability_random_",
#                                gsub(":", "_", Sys.time()),".rds"))
# # ### SAVE FOR MULTIPLE REFERENCE POPULATIONS 
# # saveRDS(cpue_abund,file=paste0("_output/cpue_abund_samp_type_",samp_type,
# #                                "_catchability_random_",
# #                                gsub(":", "_", Sys.time()),".rds"))
# proc.time()-ptm
# #user     system      elapsed 
# #0.94     0.08        204.99 
# 
# 
# ################################
# ##  TEST GET.TRND & GET.ABUND ##
# ################################
# sim_dat<-readRDS("output/catch_dat_catchability_random_rep2017-07-07 11_43_38.rds")
# 
# get_trnd<-get.trnd(sim_dat)
# get_trnd
# 
# get_abund<-get.abund(sim_dat,bends=bends)
# head(get_abund)
