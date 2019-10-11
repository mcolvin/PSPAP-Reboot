#######################
#  OVERALL UTILITIES  #
#######################
# system(paste0(Sys.getenv("R_HOME"), "/bin/x64/R.exe"), wait = FALSE, invisible = FALSE)

## FUDAMENTAL OBJECTIVE 2
datT<-readRDS("_output/4-tables/trnd_table.rds")
datT<-datT[which(datT$gear %in% c("GN14", "TLC1", "TN")),]
datT<-datT[-which(datT$estimator=="CRDMS" & datT$occasions==4),]
#datT<-datT[which(datT$catch_id==3 | datT$catch_id==4),]
datT$estimator<-factor(as.character(datT$estimator), 
                       levels=c("CPUE","M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                                "Mt_AM",  "Mt_WM",  "CRDMS"))
datT<-datT[order(datT$estimator),]
datT$trend_utility<-(0.5*datT$trend_bias_utility+0.5*datT$trend_precision_utility)
datT<-datT[,c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator",
              "reliability", "pval", "flags","effort", "q_mean_realized", "q_sd_realized",
              "q_mean_input", "B0_sd_input", "recruits_UB", "r_years_UB", "UB_recruit",
              "UB_r_freq", "recruits_LB", "r_years_LB", "LB_recruit", "LB_r_freq", "mv_beta",
              "exp_bias", "exp_precision", "trend_bias_utility", "trend_precision_utility",
              "trend_utility")]
datT[which(is.na(datT$trend_utility)),]$trend_utility<-0
names(datT)[c(7,24,25)]<-c("trend_reliability", "trend_bias","trend_precision")
datT_CPUE<-datT[which(datT$estimator=="CPUE"),]
datT_Abund<-datT[which(datT$estimator!="CPUE"),]

datA<-readRDS("_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
datA<-datA[which(datA$gear %in% c("GN14", "TLC1", "TN")),]
datA<-datA[-which(datA$estimator=="CRDMS" & datA$occasions==4),]
#datA<-datA[which(datA$catch_id==3 | datA$catch_id==4),]
datA$estimator<-factor(as.character(datA$estimator), 
                       levels=c("M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                                "Mt_AM",  "Mt_WM",  "CRDMS"))
datA<-datA[order(datA$estimator),]
datA$abund_utility<-(0.5*datA$abund_bias_utility+0.5*datA$abund_prec_utility)
datA<-datA[,c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator",
              "abs_rel_bias", "prec", "reliability", "abund_bias_utility",
              "abund_prec_utility","abund_utility")]
datA[which(is.na(datA$abund_utility)),]$abund_utility<-0
names(datA)[c(7:9,11)]<-c("abund_bias","abund_precision", "abund_reliability", "abund_precision_utility")
datA_MKA_WM<-datA[which(datA$estimator=="MKA_WM"),
                  c("pop_id", "catch_id","gear", "samp_type", "occasions",
                    "abund_bias", "abund_precision", "abund_reliability", "abund_bias_utility",
                    "abund_precision_utility","abund_utility")]

dat<-merge(datT_Abund, datA, 
           by=c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator"), 
           all=TRUE)
dat_CPUE<-merge(datT_CPUE, datA_MKA_WM,
                by=c("pop_id", "catch_id","gear", "samp_type", "occasions"), 
                all=TRUE)
dat<-rbind(dat_CPUE, dat)
dat$utility<-(0.5*dat$trend_utility+0.5*dat$abund_utility)
dat$estimator<-factor(as.character(dat$estimator), 
                      levels=c("CPUE", "M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                               "Mt_AM",  "Mt_WM",  "CRDMS"))
dat<-dat[order(datA$estimator),]

# ADD IN CPUE TREND  WITH ALL
names(datT_CPUE)[6]<-"trend_estimator"
names(datA)[6]<-"abund_estimator"
dat_CPUE_Abund<-merge(datT_CPUE, datA, 
                      by=c("pop_id", "catch_id","gear", "samp_type", "occasions"),
                      all=TRUE)
dat_CPUE_Abund$utility<-(0.5*dat_CPUE_Abund$trend_utility+0.5*dat_CPUE_Abund$abund_utility)
dat_CPUE_Abund$estimator<-paste0(dat_CPUE_Abund$trend_estimator, "+", 
                                 dat_CPUE_Abund$abund_estimator)
dat_CPUE_Abund$estimator<-factor(as.character(dat_CPUE_Abund$estimator), 
                                 levels=c("CPUE+M0_AM", "CPUE+M0_WM", "CPUE+MKA_AM",
                                          "CPUE+MKA_WM","CPUE+Mt_AM",  "CPUE+Mt_WM",
                                          "CPUE+CRDMS"))
dat_CPUE_Abund<-dat_CPUE_Abund[order(dat_CPUE_Abund$estimator),]
dat_CPUE_Abund<-dat_CPUE_Abund[,-c(6, 29)]
dat_CPUE_Abund<-dat_CPUE_Abund[,names(dat)]
dat<-rbind(dat, dat_CPUE_Abund)
names(dat)[35]<-"FO_2_Utility"


## FUNDAMENTAL OBJECTIVE 3
#dat$FO_3_Utility<-ifelse(dat$samp_type=="r", 1/6*(2/3+1+1+1+1+1), 0.2*(2/3+1+1+1/10+1+1))
dat$FO_3_Utility<-ifelse(dat$samp_type=="r", 1, 1/10)


## FUNDAMENTAL OBJECTIVE 4
dat$FO_4_Utility<-NA
dat[dat$estimator=="CPUE",]$FO_4_Utility<-2.75/18#9.75/32
dat[dat$estimator=="CRDMS" | dat$estimator=="CPUE+CRDMS",]$FO_4_Utility<-5/18#12/32
dat[dat$estimator!="CPUE" & dat$estimator!="CRDMS" & dat$estimator!="CPUE+CRDMS",]$FO_4_Utility<-4.75/18#11.75/32
dat$FO_4_Utility<- (dat$abund_reliability*dat$FO_4_Utility+(1-dat$abund_reliability)*(18*dat$FO_4_Utility-2)/18)  #(dat$abund_reliability*dat$FO_4_Utility+(1-dat$abund_reliability)*(32*dat$FO_4_Utility-2)/32)
### NEED TO ADD IN WHEN SURVIVAL IS NOT ABLE TO BE ESTIMATED

write.csv(dat, "_output/replicate_FOs_2_3_4.csv", row.names=FALSE)


## FUNDAMENTAL OBJECTIVE 1
outcomes<- read.csv("_output/age1-detection-cpt.csv")
outcomes<-outcomes[,c(2, 4:8, 10, 11, 13)]
### REPORT SAYS RECRUITMENT BETWEEN 1-1000 SO:
outcomes<-outcomes[outcomes$recruitmentLevelLabs!="1000-5000",]
outcomes$samp_type<-ifelse(outcomes$design=="Fixed", "f", "r")
outcomes<-outcomes[,c(10,2:9)]
### ASSUMING WE USE THE SAME DESIGN  IN THE UPPER AND LOWER BASINS AND WEIGHT RESULTS EVENLY
# outcomes<-ddply(outcomes, .(samp_type, ntrawlsLabs, intLocation, 
#                             recruitmentLevelLabs, pDetectLabs), summarize,
#                 FO_1_Utility=mean(detected_prob))
# dat<-dat[dat$estimator!="CPUE",]
# nrow(dat[dat$samp_type=="f",])*nrow(outcomes)/2+nrow(dat[dat$samp_type=="r",])*nrow(outcomes)/2
# dat_full<-merge(dat,outcomes, by="samp_type", all=TRUE)

### DATA TOO BIG TO COMBINE ALL REPLICATES TOGETHER SO USE REPLICATE MEANS INSTEAD
results<-ddply(dat, .(gear, samp_type, occasions, estimator),summarize,
               U_2=mean(FO_2_Utility),
               U_3=mean(FO_3_Utility),
               U_4=mean(FO_4_Utility),
               U_2_3_4_wmed=median(1/4*(FO_2_Utility+FO_3_Utility+FO_4_Utility)))
results2<-ddply(outcomes, .(samp_type, ntrawlsLabs), summarize,
                U_1=mean(detected_prob),
                U_1_wmed=median(1/4*detected_prob))
results2<-rbind(results2, data.frame(samp_type=c("f", "r"), ntrawlsLabs=as.factor(c(0,0)),
                                     U_1=c(0,0), U_1_wmed=c(0,0)))

  # NOTE:  WEIGHTS UPPER AND LOWER THE SAME SINCE NREPS ARE EQUAL :)
results<-merge(results,results2, by="samp_type", all=TRUE)
results$Utility<-1/4*(results$U_1+results$U_2+results$U_3+results$U_4)
results$U_median<-results$U_2_3_4_wmed+results$U_1_wmed
results<-results[,c("gear", "samp_type", "estimator", "occasions", 
                    "ntrawlsLabs", "U_1", "U_2", "U_3", "U_4", "Utility", "U_median")]
# NOTE: overall mean utility is the same as the mean of 
#     dat$FO_2_3_4_Utility<-1/4*(dat$FO_2_Utility+dat$FO_3_Utility+dat$FO_4_Utility)
#   plus the mean of
#     1/4*outcomes$detected_prob



## OVERALL UTILITY VS. COST
results$expected_cost<-ifelse(results$ntrawlsLabs!="0", (results$occasions+1)*0.3011479, results$occasions*0.3011479)

write.csv(results, "_output/overall_cost_and_utility.csv", row.names=FALSE)


