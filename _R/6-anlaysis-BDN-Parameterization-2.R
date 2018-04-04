# BDN TABLES


#############
#   TREND   #
#############
datT<-readRDS("_output/4-tables/trnd_table.rds")
datT<-datT[which(datT$gear %in% c("GN14", "TLC1", "TN")),]
datT<-datT[-which(datT$estimator=="CRDMS" & datT$occasions==4),]
datT[which(datT$reliability==0),]$trend_bias_utility<-0
datT[which(datT$reliability==0),]$trend_precision_utility<-0

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


## ORDER FOR NETICA TO COPY AND PASTE
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
## ORDER FOR NETICA TO COPY AND PASTE
T_bias<-T_bias[order(T_bias$samp_type,
                   T_bias$estimator,
                   T_bias$occasions,
                   T_bias$gear,
                   T_bias$q_in,
                   T_bias$B0_sd_in,
                   T_bias$rec,
                   T_bias$mov),]
## SAVE
write.csv(T_bias, "_output/4-tables/trnd_bias_cpt.csv", row.names = FALSE)

## PRECISION
T_prec<-dcast(datT,samp_type+estimator+occasions+gear+q_in+B0_sd_in+rec+mov~tpu,value.var="freq",
              fun.aggregate=sum,drop=FALSE)
T_prec$samp_size<-rowSums(T_prec[,9:24])
T_prec[which(T_prec$samp_size!=0),9:24] <- T_prec[which(T_prec$samp_size!=0),9:24]/T_prec[which(T_prec$samp_size!=0),25]
## ORDER FOR NETICA TO COPY AND PASTE
T_prec<-T_prec[order(T_prec$samp_type,
                     T_prec$estimator,
                     T_prec$occasions,
                     T_prec$gear,
                     T_prec$q_in,
                     T_prec$B0_sd_in,
                     T_prec$rec,
                     T_prec$mov),]
## SAVE
write.csv(T_prec, "_output/4-tables/trnd_precision_cpt.csv", row.names = FALSE)


