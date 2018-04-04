# BDN TABLES


##########################################
#                 TREND                  #
##########################################
datT<-readRDS("_output/4-tables/trnd_table.rds")
datT<-datT[which(datT$gear %in% c("GN14", "TLC1", "TN")),]
datT<-datT[-which(datT$estimator=="CRDMS" & datT$occasions==4),]
datT[which(datT$reliability==0),]$trend_bias_utility<-0
datT[which(datT$reliability==0),]$trend_precision_utility<-0


## BIN UP UNCERTAINTIES TO MATCH NETICA
datT$q_in<-NA
datT[datT$q_mean_input>0 & datT$q_mean_input<=0.0015,]$q_in<-"0 to 0.0015"
datT[datT$q_mean_input>0.0015 & datT$q_mean_input<=0.003,]$q_in<-"0.0015 to 0.003"
datT[datT$q_mean_input>0.003 & datT$q_mean_input<=0.0045,]$q_in<-"0.003 to 0.0045"
datT[datT$q_mean_input>0.0045 & datT$q_mean_input<=0.006,]$q_in<-"0.0045 to 0.006"
#length(which(is.na(datT$q_in)))

datT$B0_sd_in<-NA
datT[datT$B0_sd_input>0 & datT$B0_sd_input<=0.5,]$B0_sd_in<-"Low"
datT[datT$B0_sd_input>0.5 & datT$B0_sd_input<=1,]$B0_sd_in<-"Moderate"
datT[datT$B0_sd_input>1 & datT$B0_sd_input<=1.5,]$B0_sd_in<-"High"
#length(which(is.na(datT$B0_sd_in)))


#LB_recruit:  Input mean number of recruits
#recruits_LB:  Total number of recruits in the 10 years.
#LB_r_period:  Input period between reproductive events
#r_years_LB:  The number of recruitment years (out of 10). 
datT$rec<-ifelse(datT$LB_recruit>0 | datT$UB_recruit>0, "Low", "None")

datT$mov<-ifelse(is.na(datT$mv_beta), "None","Little")

# datT$q_real<-NA
# datT[datT$q_mean_realized>0 & datT$q_mean_realized<=0.0015,]$q_real<-"0 to 0.0015"
# datT[datT$q_mean_realized>0.0015 & datT$q_mean_realized<=0.003,]$q_real<-"0.0015 to 0.003"
# datT[datT$q_mean_realized>0.003 & datT$q_mean_realized<=0.0045,]$q_real<-"0.003 to 0.0045"
# datT[datT$q_mean_realized>0.0045 & datT$q_mean_realized<=0.006,]$q_real<-"0.0045 to 0.006"
# datT[datT$q_mean_realized>0.006 & datT$q_mean_realized<=0.0075,]$q_real<-"0.006 to 0.0075"
# datT[datT$q_mean_realized>0.0075 & datT$q_mean_realized<=0.009,]$q_real<-"0.0075 to 0.009"
# datT[datT$q_mean_realized>0.009 & datT$q_mean_realized<=0.0105,]$q_real<-"0.009 to 0.0105"
# datT[datT$q_mean_realized>0.0105 & datT$q_mean_realized<=0.012,]$q_real<-"0.0105 to 0.012"
# datT[datT$q_mean_realized>0.012 & datT$q_mean_realized<=0.0135,]$q_real<-"0.012 to 0.0135"
# datT[datT$q_mean_realized>0.0135 & datT$q_mean_realized<=0.015,]$q_real<-"0.0135 to 0.015"
# datT[datT$q_mean_realized>0.015 & datT$q_mean_realized<=0.0165,]$q_real<-"0.015 to 0.0165"
# #length(which(is.na(datT$q_real)))
# 
# 
# datT$q_sd_real<-NA
# datT[datT$q_sd_realized>0 & datT$q_sd_realized<=0.012,]$q_sd_real<-"Low"
# datT[datT$q_sd_realized>0.012 & datT$q_sd_realized<=0.024,]$q_sd_real<-"Moderate"
# datT[datT$q_sd_realized>0.024 & datT$q_sd_realized<=0.036,]$q_sd_real<-"High"
# #length(which(is.na(datT$q_sd_real)))

## GET DESCRITIZED BIAS AND PRECISION BINS
datT$tbu<-NA
datT[datT$trend_bias_utility==0,]$tbu<-0
datT[which(datT$trend_bias_utility>0 & datT$trend_bias_utility<=0.1),]$tbu<-"0 to 0.1"
datT[which(datT$trend_bias_utility>0.1 & datT$trend_bias_utility<=0.2),]$tbu<-"0.1 to 0.2"
datT[which(datT$trend_bias_utility>0.2 & datT$trend_bias_utility<=0.3),]$tbu<-"0.2 to 0.3"
datT[which(datT$trend_bias_utility>0.3 & datT$trend_bias_utility<=0.4),]$tbu<-"0.3 to 0.4"
datT[which(datT$trend_bias_utility>0.4 & datT$trend_bias_utility<=0.5),]$tbu<-"0.4 to 0.5"
datT[which(datT$trend_bias_utility>0.5 & datT$trend_bias_utility<=0.6),]$tbu<-"0.5 to 0.6"
datT[which(datT$trend_bias_utility>0.6 & datT$trend_bias_utility<=0.7),]$tbu<-"0.6 to 0.7"
datT[which(datT$trend_bias_utility>0.7 & datT$trend_bias_utility<=0.8),]$tbu<-"0.7 to 0.8"
datT[which(datT$trend_bias_utility>0.8 & datT$trend_bias_utility<=0.9),]$tbu<-"0.8 to 0.9"
datT[which(datT$trend_bias_utility>0.9 & datT$trend_bias_utility<=0.94),]$tbu<-"0.9 to 0.94"
datT[which(datT$trend_bias_utility>0.94 & datT$trend_bias_utility<=0.96),]$tbu<-"0.94 to 0.96"
datT[which(datT$trend_bias_utility>0.96 & datT$trend_bias_utility<=0.97),]$tbu<-"0.96 to 0.97"
datT[which(datT$trend_bias_utility>0.97 & datT$trend_bias_utility<=0.98),]$tbu<-"0.97 to 0.98"
datT[which(datT$trend_bias_utility>0.98 & datT$trend_bias_utility<=0.99),]$tbu<-"0.98 to 0.99"
datT[which(datT$trend_bias_utility>0.99 & datT$trend_bias_utility<=0.995),]$tbu<-"0.99 to 0.995"
datT[which(datT$trend_bias_utility>0.995 & datT$trend_bias_utility<=1),]$tbu<-"0.995 to 1"
#length(which(is.na(datT$tbu)))

datT$tpu<-NA
datT[which(datT$trend_precision_utility==0),]$tpu<-0
datT[which(datT$trend_precision_utility>0 & datT$trend_precision_utility<=0.1),]$tpu<-"0 to 0.1"
datT[which(datT$trend_precision_utility>0.1 & datT$trend_precision_utility<=0.2),]$tpu<-"0.1 to 0.2"
datT[which(datT$trend_precision_utility>0.2 & datT$trend_precision_utility<=0.3),]$tpu<-"0.2 to 0.3"
datT[which(datT$trend_precision_utility>0.3 & datT$trend_precision_utility<=0.4),]$tpu<-"0.3 to 0.4"
datT[which(datT$trend_precision_utility>0.4 & datT$trend_precision_utility<=0.5),]$tpu<-"0.4 to 0.5"
datT[which(datT$trend_precision_utility>0.5 & datT$trend_precision_utility<=0.6),]$tpu<-"0.5 to 0.6"
datT[which(datT$trend_precision_utility>0.6 & datT$trend_precision_utility<=0.7),]$tpu<-"0.6 to 0.7"
datT[which(datT$trend_precision_utility>0.7 & datT$trend_precision_utility<=0.8),]$tpu<-"0.7 to 0.8"
datT[which(datT$trend_precision_utility>0.8 & datT$trend_precision_utility<=0.9),]$tpu<-"0.8 to 0.9"
datT[which(datT$trend_precision_utility>0.9 & datT$trend_precision_utility<=0.94),]$tpu<-"0.9 to 0.94"
datT[which(datT$trend_precision_utility>0.94 & datT$trend_precision_utility<=0.96),]$tpu<-"0.94 to 0.96"
datT[which(datT$trend_precision_utility>0.96 & datT$trend_precision_utility<=0.97),]$tpu<-"0.96 to 0.97"
datT[which(datT$trend_precision_utility>0.97 & datT$trend_precision_utility<=0.98),]$tpu<-"0.97 to 0.98"
datT[which(datT$trend_precision_utility>0.98 & datT$trend_precision_utility<=0.99),]$tpu<-"0.98 to 0.99"
datT[which(datT$trend_precision_utility>0.99 & datT$trend_precision_utility<=1),]$tpu<-"0.99 to 1"
#length(which(is.na(datT$tpu)))

datT$freq<-1

## ORDERING
datT$samp_type<- factor(as.character(datT$samp_type),
                         levels=c("f","r"),
                         ordered=TRUE)
datT$estimator<- factor(as.character(datT$estimator),
                        levels=c("CPUE","MKA_AM","MKA_WM","M0_AM","M0_WM", "Mt_AM",
                                 "Mt_WM","CRDMS"),
                        ordered=TRUE)
datT$gear<- factor(as.character(datT$gear),
                   levels=c("TLC1","TN", "GN14"),
                   labels=c("Trotlines", "Trammel Nets", "Gill Nets"),
                   ordered=TRUE)
datT$q_in<- factor(as.character(datT$q_in),
                   levels=c("0 to 0.0015","0.0015 to 0.003","0.003 to 0.0045",
                            "0.0045 to 0.006"),
                   ordered=TRUE)
datT$B0_sd_in<- factor(as.character(datT$B0_sd_in),
                       levels=c("Low","Moderate","High"),
                       ordered=TRUE)
datT$rec<- factor(as.character(datT$rec),
                  levels=c("None","Low"),
                  ordered=TRUE)
datT$mov<- factor(as.character(datT$mov),
                  levels=c("None","Little"),
                  ordered=TRUE)
datT<-datT[order(datT$samp_type,
                 datT$estimator,
                 datT$occasions,
                 datT$gear,
                 datT$q_in,
                 datT$B0_sd_in,
                 datT$rec,
                 datT$mov),]

## BIAS
T_bias<-dcast(datT,samp_type+estimator+occasions+gear+q_in+B0_sd_in+rec+mov~tbu,value.var="freq",
              fun.aggregate=sum,drop=FALSE)
T_bias$samp_size<-rowSums(T_bias[,9:25])
T_bias[which(T_bias$samp_size!=0),9:25] <- T_bias[which(T_bias$samp_size!=0),9:25]/T_bias[which(T_bias$samp_size!=0),26]
### ORDER FOR NETICA TO COPY AND PASTE
T_bias<-T_bias[order(T_bias$samp_type,
                   T_bias$estimator,
                   T_bias$occasions,
                   T_bias$gear,
                   T_bias$q_in,
                   T_bias$B0_sd_in,
                   T_bias$rec,
                   T_bias$mov),]
### SAVE
write.csv(T_bias, "_output/4-tables/trnd_bias_cpt.csv", row.names = FALSE)

## PRECISION
T_prec<-dcast(datT,samp_type+estimator+occasions+gear+q_in+B0_sd_in+rec+mov~tpu,value.var="freq",
              fun.aggregate=sum,drop=FALSE)
T_prec$samp_size<-rowSums(T_prec[,9:24])
T_prec[which(T_prec$samp_size!=0),9:24] <- T_prec[which(T_prec$samp_size!=0),9:24]/T_prec[which(T_prec$samp_size!=0),25]
### ORDER FOR NETICA TO COPY AND PASTE
T_prec<-T_prec[order(T_prec$samp_type,
                     T_prec$estimator,
                     T_prec$occasions,
                     T_prec$gear,
                     T_prec$q_in,
                     T_prec$B0_sd_in,
                     T_prec$rec,
                     T_prec$mov),]
### SAVE
write.csv(T_prec, "_output/4-tables/trnd_precision_cpt.csv", row.names = FALSE)


##############################################
#                 ABUNDANCE                  #
##############################################
datA<-readRDS("_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
datA<-datA[which(datA$gear %in% c("GN14", "TLC1", "TN")),]
datA<-datA[-which(datA$estimator=="CRDMS" & datA$occasions==4),]
datA[which(datA$reliability==0),]$abund_bias_utility<-0
datA[which(datA$reliability==0),]$abund_prec_utility<-0

datCPUE<-datA[datA$estimator=="MKA_WM",]
datCPUE$estimator<-"CPUE"
datA<-rbind(datA,datCPUE)
rm(datCPUE)

## ORDERING
datA$samp_type<- factor(as.character(datA$samp_type),
                        levels=c("f","r"),
                        ordered=TRUE)
datA$estimator<- factor(as.character(datA$estimator),
                        levels=c("CPUE","MKA_AM","MKA_WM","M0_AM","M0_WM", "Mt_AM",
                                 "Mt_WM","CRDMS"),
                        ordered=TRUE)
datA$gear<- factor(as.character(datA$gear),
                   levels=c("TLC1","TN", "GN14"),
                   labels=c("Trotlines", "Trammel Nets", "Gill Nets"),
                   ordered=TRUE)

## ADD IN BINNED UNCERTAINTY VALUES (TO MATCH NETICA)
datA<-merge(datA, datT[, c("samp_type", "pop_id", "catch_id", "estimator", "occasions",
                     "gear", "q_in", "B0_sd_in", "rec", "mov")],
      by=c("samp_type", "pop_id", "catch_id", "estimator", "occasions",
           "gear"), all.x=TRUE)


## GET DESCRITIZED BIAS AND PRECISION BINS
datA$abu<-NA
datA[datA$abund_bias_utility==0,]$abu<-0
datA[which(datA$abund_bias_utility>0 & datA$abund_bias_utility<=0.1),]$abu<-"0 to 0.1"
datA[which(datA$abund_bias_utility>0.1 & datA$abund_bias_utility<=0.2),]$abu<-"0.1 to 0.2"
datA[which(datA$abund_bias_utility>0.2 & datA$abund_bias_utility<=0.3),]$abu<-"0.2 to 0.3"
datA[which(datA$abund_bias_utility>0.3 & datA$abund_bias_utility<=0.4),]$abu<-"0.3 to 0.4"
datA[which(datA$abund_bias_utility>0.4 & datA$abund_bias_utility<=0.5),]$abu<-"0.4 to 0.5"
datA[which(datA$abund_bias_utility>0.5 & datA$abund_bias_utility<=0.6),]$abu<-"0.5 to 0.6"
datA[which(datA$abund_bias_utility>0.6 & datA$abund_bias_utility<=0.7),]$abu<-"0.6 to 0.7"
datA[which(datA$abund_bias_utility>0.7 & datA$abund_bias_utility<=0.8),]$abu<-"0.7 to 0.8"
datA[which(datA$abund_bias_utility>0.8 & datA$abund_bias_utility<=0.9),]$abu<-"0.8 to 0.9"
datA[which(datA$abund_bias_utility>0.9 & datA$abund_bias_utility<=0.95),]$abu<-"0.9 to 0.95"
datA[which(datA$abund_bias_utility>0.95 & datA$abund_bias_utility<=1),]$abu<-"0.95 to 1"
#length(which(is.na(datA$abu)))

datA$apu<-NA
datA[datA$abund_prec_utility==0,]$apu<-0
datA[which(datA$abund_prec_utility>0 & datA$abund_prec_utility<=0.1),]$apu<-"0 to 0.1"
datA[which(datA$abund_prec_utility>0.1 & datA$abund_prec_utility<=0.2),]$apu<-"0.1 to 0.2"
datA[which(datA$abund_prec_utility>0.2 & datA$abund_prec_utility<=0.3),]$apu<-"0.2 to 0.3"
datA[which(datA$abund_prec_utility>0.3 & datA$abund_prec_utility<=0.4),]$apu<-"0.3 to 0.4"
datA[which(datA$abund_prec_utility>0.4 & datA$abund_prec_utility<=0.5),]$apu<-"0.4 to 0.5"
datA[which(datA$abund_prec_utility>0.5 & datA$abund_prec_utility<=0.6),]$apu<-"0.5 to 0.6"
datA[which(datA$abund_prec_utility>0.6 & datA$abund_prec_utility<=0.7),]$apu<-"0.6 to 0.7"
datA[which(datA$abund_prec_utility>0.7 & datA$abund_prec_utility<=0.8),]$apu<-"0.7 to 0.8"
datA[which(datA$abund_prec_utility>0.8 & datA$abund_prec_utility<=0.9),]$apu<-"0.8 to 0.9"
datA[which(datA$abund_prec_utility>0.9 & datA$abund_prec_utility<=0.95),]$apu<-"0.9 to 0.95"
datA[which(datA$abund_prec_utility>0.95 & datA$abund_prec_utility<=0.98),]$apu<-"0.95 to 0.98"
datA[which(datA$abund_prec_utility>0.98 & datA$abund_prec_utility<=0.995),]$apu<-"0.98 to 0.995"
datA[which(datA$abund_prec_utility>0.995 & datA$abund_prec_utility<=1),]$apu<-"0.995 to 1"
#length(which(is.na(datA$apu)))


datA$aru<-NA
datA[datA$reliability==0,]$aru<-0
datA[which(datA$reliability>0 & datA$reliability<=0.1),]$aru<-"0 to 0.1"
datA[which(datA$reliability>0.1 & datA$reliability<=0.2),]$aru<-"0.1 to 0.2"
datA[which(datA$reliability>0.2 & datA$reliability<=0.3),]$aru<-"0.2 to 0.3"
datA[which(datA$reliability>0.3 & datA$reliability<=0.4),]$aru<-"0.3 to 0.4"
datA[which(datA$reliability>0.4 & datA$reliability<=0.5),]$aru<-"0.4 to 0.5"
datA[which(datA$reliability>0.5 & datA$reliability<=0.6),]$aru<-"0.5 to 0.6"
datA[which(datA$reliability>0.6 & datA$reliability<=0.7),]$aru<-"0.6 to 0.7"
datA[which(datA$reliability>0.7 & datA$reliability<=0.8),]$aru<-"0.7 to 0.8"
datA[which(datA$reliability>0.8 & datA$reliability<=0.9),]$aru<-"0.8 to 0.9"
datA[which(datA$reliability>0.9 & datA$reliability<=0.995),]$aru<-"0.9 to 0.995"
datA[which(datA$reliability>0.995 & datA$reliability<=1),]$aru<-"0.995 to 1"
#length(which(is.na(datA$aru)))


## ORDER FOR NETICA TO COPY AND PASTE
datA<-datA[order(datA$samp_type,
                 datA$estimator,
                 datA$occasions,
                 datA$gear,
                 datA$q_in,
                 datA$B0_sd_in,
                 datA$rec,
                 datA$mov),]




datA$freq<-1

## BIAS
A_bias<-dcast(datA,samp_type+estimator+occasions+gear+q_in+B0_sd_in+rec+mov~abu,value.var="freq",
              fun.aggregate=sum,drop=FALSE)
A_bias$samp_size<-rowSums(A_bias[,9:20])
A_bias[which(A_bias$samp_size!=0),9:20] <- A_bias[which(A_bias$samp_size!=0),9:20]/A_bias[which(A_bias$samp_size!=0),21]
## ORDER FOR NETICA TO COPY AND PASTE
A_bias<-A_bias[order(A_bias$samp_type,
                     A_bias$estimator,
                     A_bias$occasions,
                     A_bias$gear,
                     A_bias$q_in,
                     A_bias$B0_sd_in,
                     A_bias$rec,
                     A_bias$mov),]
## SAVE
write.csv(A_bias, "_output/4-tables/abund_bias_cpt.csv", row.names = FALSE)

## PRECISION
A_prec<-dcast(datA,samp_type+estimator+occasions+gear+q_in+B0_sd_in+rec+mov~apu,value.var="freq",
              fun.aggregate=sum,drop=FALSE)
A_prec$samp_size<-rowSums(A_prec[,9:22])
A_prec[which(A_prec$samp_size!=0),9:22] <- A_prec[which(A_prec$samp_size!=0),9:22]/A_prec[which(A_prec$samp_size!=0),23]
## ORDER FOR NETICA TO COPY AND PASTE
A_prec<-A_prec[order(A_prec$samp_type,
                     A_prec$estimator,
                     A_prec$occasions,
                     A_prec$gear,
                     A_prec$q_in,
                     A_prec$B0_sd_in,
                     A_prec$rec,
                     A_prec$mov),]
## SAVE
write.csv(A_prec, "_output/4-tables/abund_precision_cpt.csv", row.names = FALSE)


## RELIABILITY
A_reli<-dcast(datA,samp_type+estimator+occasions+gear+q_in+B0_sd_in+rec+mov~aru,value.var="freq",
              fun.aggregate=sum,drop=FALSE)
A_reli$samp_size<-rowSums(A_reli[,9:20])
A_reli[which(A_reli$samp_size!=0),9:20] <- A_reli[which(A_reli$samp_size!=0),9:20]/A_reli[which(A_reli$samp_size!=0),21]
## ORDER FOR NETICA TO COPY AND PASTE
A_reli<-A_reli[order(A_reli$samp_type,
                     A_reli$estimator,
                     A_reli$occasions,
                     A_reli$gear,
                     A_reli$q_in,
                     A_reli$B0_sd_in,
                     A_reli$rec,
                     A_reli$mov),]
## SAVE
write.csv(A_reli, "_output/4-tables/abund_reliability_cpt.csv", row.names = FALSE)


