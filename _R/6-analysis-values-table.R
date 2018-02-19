library(plyr)
tt<-readRDS("_output/4-tables/trnd_table.rds")
tt<-tt[,c("pop_id","catch_id","samp_type", "occasions", "gear", "estimator", "q_mean_input",
          "B0_sd_input", "q_mean_realized", "q_sd_realized", "recruits_UB", "r_years_UB", 
          "recruits_LB", "r_years_LB", "exp_bias", "exp_precision")]
names(tt)[15]<-"trend_bias"
names(tt)[16]<-"trend_precision"

tt2<-ddply(tt, .(samp_type, occasions, gear, estimator), summarize,
           mean_trend_bias=mean(trend_bias, na.rm = TRUE),
           mean_trend_precision=mean(trend_precision, na.rm = TRUE))
tt2<-tt2[which(tt2$gear %in% c("GN14", "TLC1", "TN")),]

dat_list<-dir("_output/4-tables/", ".rds")
dat_list<-dat_list[c(1:7)]
tbl2<-lapply(dat_list, function(x)
{
  dat<-readRDS(paste0("_output/4-tables/", x))
  
  
  tmp<-ddply(dat, 
             .(samp_type, occasions, gear, estimator), 
             summarize, 
             mean_abundance_bias=mean(rel_bias, na.rm=TRUE), 
             mean_abundance_precision=mean(prec, na.rm=TRUE))
  return(tmp)
})
tbl2<-do.call(rbind,tbl2)

tbl2<-tbl2[which(tbl2$gear %in% c("GN14", "TLC1", "TN")),]

final2<-merge(tt2, tbl2, 
              by=c("samp_type", "occasions", "gear", "estimator"), 
              all=TRUE)
# ADD CPUE ABUNDANCE = MKA_WM ABUNDANCE
dat<-final2[which(final2$estimator=="CPUE"), c(1:6)]
dat2<-final2[which(final2$estimator=="MKA_WM"), c(1:3,7,8)]
dat<-merge(dat,dat2, by=c("samp_type", "occasions", "gear"),all=TRUE)
dat2<-final2[which(final2$estimator!="CPUE"),]
final2<-rbind(dat2, dat)
final2<-final2[order(final2$samp_type,final2$occasions, final2$estimator),]

finalGN<-final2[which(final2$gear=="GN14"),]
finalTL<-final2[which(final2$gear=="TLC1"),]
finalTN<-final2[which(final2$gear=="TN"),]


write.csv(finalGN, "_output/values_GN.csv", row.names = FALSE)
write.csv(finalTL, "_output/values_TL.csv", row.names = FALSE)
write.csv(finalTN, "_output/values_TN.csv", row.names = FALSE)