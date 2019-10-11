# WORST AND BEST CASE SCENARIOS FOR EACH METRIC


## ABUNDANCE
### SEGMENT ABUNDANCE
system(paste0(Sys.getenv("R_HOME"), "/bin/x64/R.exe"), wait = FALSE, invisible = FALSE)

library(plyr)

dat_list<-dir("D:/_output/4-tables/", ".rds")
dat_list<-dat_list[c(2:8)]
tbl<-lapply(dat_list, function(x)
{
  dat<-readRDS(paste0("D:/_output/4-tables/", x))
  dat$rel_bias<-abs(dat$bias)/dat$abundance
  
  tmp<-ddply(dat, .(samp_type, gear, pop_id, catch_id, occasions), summarize,
             bias=mean(bias, na.rm=TRUE),
             rel_bias=mean(rel_bias, na.rm=TRUE),
             prec=mean(precision, na.rm=TRUE),
             sd=mean(precision*Nhat, na.rm=TRUE),
             reliability=mean(reliability, na.rm=TRUE),
             min_abund=min(abundance),
             mean_abund=mean(abundance),
             max_abund=max(abundance))
  tmp$estimator<-unique(dat$estimator)
  saveRDS(tmp, paste0("D:/_output/4-tables/Worst_Best/AVG_SEG_ABUND_", x))
  
  out<-ddply(tmp, .(samp_type, estimator), summarize,
             min_bias=min(bias, na.rm=TRUE),
             min_b_id=paste0(samp_type[which.min(bias)], "_", pop_id[which.min(bias)], "-", catch_id[which.min(bias)], "_", gear[which.min(bias)], "-", occasions[which.min(bias)]),
             max_bias=max(bias, na.rm=TRUE),
             max_b_id=paste0(samp_type[which.max(bias)], "_", pop_id[which.max(bias)], "-", catch_id[which.max(bias)], "_", gear[which.max(bias)], "-", occasions[which.max(bias)]),
             min_abs_bias=min(abs(bias), na.rm=TRUE),
             min_abs_b_id=paste0(samp_type[which.min(abs(bias))], "_", pop_id[which.min(abs(bias))], "-", catch_id[which.min(abs(bias))], "_", gear[which.min(abs(bias))], "-", occasions[which.min(bias)]),
             min_rel_bias=min(rel_bias, na.rm=TRUE),
             min_relb_id=paste0(samp_type[which.min(rel_bias)], "_", pop_id[which.min(rel_bias)], "-", catch_id[which.min(rel_bias)], "_", gear[which.min(rel_bias)], "-", occasions[which.min(rel_bias)]),
             max_rel_bias=max(rel_bias, na.rm=TRUE),
             max_relb_id=paste0(samp_type[which.max(rel_bias)], "_", pop_id[which.max(rel_bias)], "-", catch_id[which.max(rel_bias)], "_", gear[which.max(rel_bias)], "-", occasions[which.max(rel_bias)]),
             min_prec=min(prec, na.rm=TRUE),
             min_p_id=paste0(samp_type[which.min(prec)], "_", pop_id[which.min(prec)], "-", catch_id[which.min(prec)], "_", gear[which.min(prec)], "-", occasions[which.min(prec)]),
             max_prec=max(prec, na.rm=TRUE),
             max_p_id=paste0(samp_type[which.max(prec)], "_", pop_id[which.max(prec)], "-", catch_id[which.max(prec)], "_", gear[which.max(prec)], "-", occasions[which.max(prec)]),
             min_sd=min(sd, na.rm=TRUE),
             min_sd_id=paste0(samp_type[which.min(sd)], "_", pop_id[which.min(sd)], "-", catch_id[which.min(sd)], "_", gear[which.min(sd)], "-", occasions[which.min(sd)]),
             max_sd=max(sd, na.rm=TRUE),
             max_sd_id=paste0(samp_type[which.max(sd)], "_", pop_id[which.max(sd)], "-", catch_id[which.max(sd)], "_", gear[which.max(sd)], "-", occasions[which.max(sd)]),
             min_rely=min(reliability),
             max_rely=max(reliability))
  rm(dat, tmp)
  return(out)
})
tbl<-do.call(rbind, tbl)
saveRDS(tbl, "D:/_output/4-tables/Worst_Best/abund_min_max_by_samp_type_and_estimator.rds")

values<-data.frame(min_bias=min(tbl$min_bias),
                   max_bias=max(tbl$max_bias),
                   min_abs_bias=min(tbl$min_abs_bias),
                   min_prec=min(tbl$min_prec),
                   max_prec=max(tbl$max_prec),
                   min_sd=min(tbl$min_sd),
                   max_sd=max(tbl$max_sd),
                   min_rely=min(tbl$min_rely),
                   max_rely=max(tbl$max_rely),
                   min_b_id=paste0(tbl$estimator[which.min(tbl$min_bias)],"_",tbl$min_b_id[which.min(tbl$min_bias)]),
                   max_b_id=paste0(tbl$estimator[which.max(tbl$max_bias)],"_",tbl$max_b_id[which.max(tbl$max_bias)]),
                   min_abs_b_id=paste0(tbl$estimator[which.min(tbl$min_abs_bias)],"_",tbl$min_abs_b_id[which.min(tbl$min_abs_bias)]),
                   min_p_id=paste0(tbl$estimator[which.min(tbl$min_prec)],"_",tbl$min_p_id[which.min(tbl$min_prec)]),
                   max_p_id=paste0(tbl$estimator[which.max(tbl$max_prec)],"_",tbl$max_p_id[which.max(tbl$max_prec)]),
                   min_sd_id=paste0(tbl$estimator[which.min(tbl$min_sd)],"_",tbl$min_sd_id[which.min(tbl$min_sd)]),
                   max_sd_id=paste0(tbl$estimator[which.max(tbl$max_sd)],"_",tbl$max_sd_id[which.max(tbl$max_sd)]))
saveRDS(values, "D:/_output/4-tables/Worst_Best/SW_abund_min_max.rds")
values<-readRDS("D:/_output/4-tables/Worst_Best/SW_abund_min_max.rds")

### BASIN ABUNDANCE
basin_abund<-lapply(1:400, function(i)
{
  sim_pop<-readRDS(paste0("D:/_output/1-populations/sim_pop_", i, ".rds"))
  nyears<-sim_pop$inputs$nyears
  abundL<-sim_pop$out[1:315,]
  abundU<-sim_pop$out[316:470,]#[316:471,]
  out<-data.frame(basin=c(rep("L", nyears), rep("U", nyears)), year=rep(1:nyears, 2), 
                  abundance=c(colSums(abundL), colSums(abundU)), pop_id=i) 
  return(out)
})
basin_abund<-do.call(rbind, basin_abund)
min_max<-ddply(basin_abund, .(basin), summarize,
               min_abund=min(abundance),
               mean_abund=mean(abundance),
               max_abund=max(abundance))
min_max #including segment 1
#basin  min_abund   mean_abund   max_abund
# L      16558       21512.00      27191
# U      20129       26193.42      33310

min_max #excluding segment 1
#basin  min_abund   mean_abund   max_abund
#  L      16558      21512.00     27191
#  U      19706      25634.92     32574

system(paste0(Sys.getenv("R_HOME"), "/bin/x64/R.exe"), wait = FALSE, invisible = FALSE)

library(plyr)

dat_list<-dir("D:/_output/4-tables/", ".rds")
dat_list<-dat_list[c(2:8)]
pop<-readRDS("D:/_output/1-populations/sim_pop_1.rds")
seg_length<-ddply(pop$bendMeta, .(b_segment), summarize, rkm=sum(length.rkm))
names(seg_length)[1]<-"segment"
UB_rkm1<-sum(seg_length$rkm[1:4])
UB_rkm<-sum(seg_length$rkm[2:4])
LB_rkm<-sum(seg_length$rkm[5:10])
lapply(dat_list, function(x)
{
  dat<-readRDS(paste0("D:/_output/4-tables/", x))
  dat$basin<-ifelse(dat$segment %in% c(1:4), "U", "L")
  dat<-merge(dat,seg_length, by="segment", all.x=TRUE)
  if(length(which(is.na(dat$Nhat)))>0){dat[which(is.na(dat$Nhat)),]$rkm<-NA}
  
  tmp<-ddply(dat, .(basin, year, samp_type, gear, pop_id, catch_id, occasions), summarize,
             N_basin=sum(abundance),
             Nhat_estimated=sum(Nhat, na.rm=TRUE),#WHY DO WE HAVE NAs AT THE SEG LEVEL??? NO BEND ABUNDS ESTIMATED???
             rkm_estimated=sum(rkm, na.rm=TRUE),
             sd_estimated=sqrt(sum((precision*Nhat)^2, na.rm=TRUE)),
             bias_sum=sum(bias, na.rm=TRUE),
             reliability=mean(reliability, na.rm=TRUE),
             min_abund=min(abundance),
             mean_abund=mean(abundance),
             max_abund=max(abundance))
  tmp$estimator<-unique(dat$estimator)
  tmp$rkm<-ifelse(tmp$basin=="U", UB_rkm, LB_rkm)
  tmp$Nhat<-tmp$Nhat_estimated/tmp$rkm_estimated*tmp$rkm
  tmp$sd<-tmp$sd_estimated/tmp$rkm_estimated*tmp$rkm
  tmp$bias<-tmp$Nhat-tmp$N_basin
  tmp$rel_bias<-tmp$bias/tmp$N_basin
  tmp$precision<-tmp$sd/tmp$Nhat
  saveRDS(tmp, paste0("D:/_output/4-tables/Worst_Best/BASIN_YEAR_ABUND_", x))
  rm(tmp)
})
  
lapply(dat_list, function(x)
{
  tmp<-readRDS(paste0("D:/_output/4-tables/Worst_Best/BASIN_YEAR_ABUND_", x))
  tmp2<-ddply(tmp, .(basin, samp_type, gear, pop_id, catch_id, occasions), summarize,
             bias_sum=mean(bias_sum, na.rm=TRUE),
             bias=mean(bias, na.rm=TRUE),
             rel_bias=mean(rel_bias, na.rm=TRUE),
             rel_abs_bias=mean(abs(rel_bias), na.rm=TRUE),
             prec=mean(precision, na.rm=TRUE),
             sd=mean(sd, na.rm=TRUE),
             reliability=mean(reliability, na.rm=TRUE),
             mean_seg_abund_range<-mean(max_abund-min_abund))
  tmp2$estimator<-unique(tmp$estimator)
  saveRDS(tmp2, paste0("D:/_output/4-tables/Worst_Best/AVG_BASIN_ABUND_", x))
  rm(tmp2)
})

tbl<-lapply(dat_list, function(x)  
{
  tmp2<-readRDS(paste0("D:/_output/4-tables/Worst_Best/AVG_BASIN_ABUND_", x))
  out<-ddply(tmp2, .(basin, samp_type, estimator), summarize,
             min_bias=min(bias, na.rm=TRUE),
             min_b_id=paste0(samp_type[which.min(bias)], "_", pop_id[which.min(bias)], "-", catch_id[which.min(bias)], "_", gear[which.min(bias)], "-", occasions[which.min(bias)]),
             max_bias=max(bias, na.rm=TRUE),
             max_b_id=paste0(samp_type[which.max(bias)], "_", pop_id[which.max(bias)], "-", catch_id[which.max(bias)], "_", gear[which.max(bias)], "-", occasions[which.max(bias)]),
             min_abs_bias=min(abs(bias), na.rm=TRUE),
             min_abs_b_id=paste0(samp_type[which.min(abs(bias))], "_", pop_id[which.min(abs(bias))], "-", catch_id[which.min(abs(bias))], "_", gear[which.min(abs(bias))], "-", occasions[which.min(bias)]),
             min_rel_bias=min(rel_bias, na.rm=TRUE),
             min_relb_id=paste0(samp_type[which.min(rel_bias)], "_", pop_id[which.min(rel_bias)], "-", catch_id[which.min(rel_bias)], "_", gear[which.min(rel_bias)], "-", occasions[which.min(rel_bias)]),
             max_rel_bias=max(rel_bias, na.rm=TRUE),
             max_relb_id=paste0(samp_type[which.max(rel_bias)], "_", pop_id[which.max(rel_bias)], "-", catch_id[which.max(rel_bias)], "_", gear[which.max(rel_bias)], "-", occasions[which.max(rel_bias)]),
             min_rel_abs_bias=min(rel_abs_bias, na.rm=TRUE),
             min_relab_id=paste0(samp_type[which.min(rel_abs_bias)], "_", pop_id[which.min(rel_abs_bias)], "-", catch_id[which.min(rel_abs_bias)], "_", gear[which.min(rel_abs_bias)], "-", occasions[which.min(rel_abs_bias)]),
             min_prec=min(prec, na.rm=TRUE),
             min_p_id=paste0(samp_type[which.min(prec)], "_", pop_id[which.min(prec)], "-", catch_id[which.min(prec)], "_", gear[which.min(prec)], "-", occasions[which.min(prec)]),
             max_prec=max(prec, na.rm=TRUE),
             max_p_id=paste0(samp_type[which.max(prec)], "_", pop_id[which.max(prec)], "-", catch_id[which.max(prec)], "_", gear[which.max(prec)], "-", occasions[which.max(prec)]),
             min_sd=min(sd, na.rm=TRUE),
             min_sd_id=paste0(samp_type[which.min(sd)], "_", pop_id[which.min(sd)], "-", catch_id[which.min(sd)], "_", gear[which.min(sd)], "-", occasions[which.min(sd)]),
             max_sd=max(sd, na.rm=TRUE),
             max_sd_id=paste0(samp_type[which.max(sd)], "_", pop_id[which.max(sd)], "-", catch_id[which.max(sd)], "_", gear[which.max(sd)], "-", occasions[which.max(sd)]),
             min_rely=min(reliability),
             max_rely=max(reliability))
  rm(tmp2)
  return(out)
})
tbl<-do.call(rbind, tbl)
saveRDS(tbl, "D:/_output/4-tables/Worst_Best/abund_basin_min_max_by_samp_type_and_estimator.rds")
tbl<-readRDS("D:/_output/4-tables/Worst_Best/abund_basin_min_max_by_samp_type_and_estimator.rds")

values<-ddply(tbl, .(basin),summarize,
                   min_bias=min(min_bias),
                   max_bias=max(max_bias),
                   min_abs_bias=min(min_abs_bias),
                   min_rel_bias=min(min_rel_bias),
                   max_rel_bias=max(max_rel_bias),
                   min_rel_abs_bias=min(min_rel_abs_bias),
                   min_prec=min(min_prec),
                   max_prec=max(max_prec),
                   min_sd=min(min_sd),
                   max_sd=max(max_sd),
                   min_rely=min(min_rely),
                   max_rely=max(max_rely))
saveRDS(values, "D:/_output/4-tables/Worst_Best/SW_abund_basin_min_max.rds")
values<-readRDS("D:/_output/4-tables/Worst_Best/SW_abund_basin_min_max.rds")



### PLOT EXAMPLES
#### BIAS
##### ALL TOGETHER
        x<-c(values$min_bias, values$max_bias, values$min_abs_bias)
        y<-rep(0,3)
        plot(x,y, type="p", xlab="Bias (Number of Fish)", ylab="", ylim=c(-1,1), yaxt='n', main="Segment Abundance Bias: Worst & Best Cases")
        abline(v=0, lty=3)
##### BIAS VS REL. BIAS
        # dat_list<-dir("D:/_output/4-tables/", ".rds")
        # dat_list<-dat_list[c(2:8)]
        # comp<-lapply(dat_list, function(x)
        #   {
        #     dat<-readRDS(paste0("D:/_output/4-tables/Worst_Best/AVG_SEG_ABUND_", x))
        #     plot(dat$rel_bias, dat$bias, type="p")
        #     plot(dat$rel_bias, dat$bias, type="p", xlim=c(0,2), ylim=c(-20000,20000))
        #     plot(dat$rel_bias, dat$bias, type="p", xlim=c(0,1.1), ylim=c(-10000,10000))
        #     abline(h=-5000)
        #     abline(h=5000)
        #     abline(v=0.5)
        #     abline(h=4000)
        #     abline(h=3000)
        #     abline(h=-3000)
        #     abline(h=-4000)
        #     abline(v=0.8)
        #     
        #     plot(dat$abundance, dat$bias, type="p")
        #     
        
#### PRECISION
##### WORST
        worst<-values$max_prec
        pop<-readRDS("D:/_output/1-populations/sim_pop_1.rds")
        pop_abund<-ddply(pop$bendMeta, .(b_segment), summarize, 
                         seg_abund=sum(N_ini))
        abund<-mean(pop_abund$seg_abund)
        worst_sd<-worst*abund
        x<-seq(0,floor(abund+3*worst_sd),1)
        plot(x,dnorm(x, mean=abund, sd=worst_sd),ylim=c(0,0.0001), xlab="Number of Fish", ylab="Probability",main="Abundance: Worst Precision")
##### BEST...TOO GOOD TO PLOT (JUST A DOT)
        best<-values$min_prec
        best_sd<-best*abund
        


        
        
        

## TREND
dat<-readRDS("D:/_output/4-tables/trnd_table.rds")
## dat HAS ONLY ONE ESTIMATE FOR EACH samp_type, gear, pop_id, catch_id, AND occasions COMBINATION

tbl<-ddply(dat, .(samp_type, estimator), summarize,
           min_bias=min(bias, na.rm=TRUE),
           min_b_id=paste0(samp_type[which.min(bias)], "_", pop_id[which.min(bias)], "-", catch_id[which.min(bias)], "_", gear[which.min(bias)], "-", occasions[which.min(bias)]),
           max_bias=max(bias, na.rm=TRUE),
           max_b_id=paste0(samp_type[which.max(bias)], "_", pop_id[which.max(bias)], "-", catch_id[which.max(bias)], "_", gear[which.max(bias)], "-", occasions[which.max(bias)]),
           min_abs_bias=min(abs(bias), na.rm=TRUE),
           min_abs_b_id=paste0(samp_type[which.min(abs(bias))], "_", pop_id[which.min(abs(bias))], "-", catch_id[which.min(abs(bias))], "_", gear[which.min(abs(bias))], "-", occasions[which.min(bias)]),
           min_prec=min(precision, na.rm=TRUE),
           min_p_id=paste0(samp_type[which.min(precision)], "_", pop_id[which.min(precision)], "-", catch_id[which.min(precision)], "_", gear[which.min(precision)], "-", occasions[which.min(precision)]),
           max_prec=max(precision, na.rm=TRUE),
           max_p_id=paste0(samp_type[which.max(precision)], "_", pop_id[which.max(precision)], "-", catch_id[which.max(precision)], "_", gear[which.max(precision)], "-", occasions[which.max(precision)]),
           min_sd=min(precision*abs(trnd), na.rm=TRUE),
           min_sd_id=paste0(samp_type[which.min(precision*abs(trnd))], "_", pop_id[which.min(precision*abs(trnd))], "-", catch_id[which.min(precision*abs(trnd))], "_", gear[which.min(precision*abs(trnd))], "-", occasions[which.min(precision*abs(trnd))]),
           max_sd=max(precision*abs(trnd), na.rm=TRUE),
           max_sd_id=paste0(samp_type[which.max(precision*abs(trnd))], "_", pop_id[which.max(precision*abs(trnd))], "-", catch_id[which.max(precision*abs(trnd))], "_", gear[which.max(precision*abs(trnd))], "-", occasions[which.max(precision*abs(trnd))]),
           min_exp_bias=min(exp_bias, na.rm=TRUE),
           min_expb_id=paste0(samp_type[which.min(exp_bias)], "_", pop_id[which.min(exp_bias)], "-", catch_id[which.min(exp_bias)], "_", gear[which.min(exp_bias)], "-", occasions[which.min(exp_bias)]),
           max_exp_bias=max(exp_bias, na.rm=TRUE),
           max_expb_id=paste0(samp_type[which.max(exp_bias)], "_", pop_id[which.max(exp_bias)], "-", catch_id[which.max(exp_bias)], "_", gear[which.max(exp_bias)], "-", occasions[which.max(exp_bias)]),
           min_abs_exp_bias=min(abs(exp_bias), na.rm=TRUE),
           min_abs_expb_id=paste0(samp_type[which.min(abs(exp_bias))], "_", pop_id[which.min(abs(exp_bias))], "-", catch_id[which.min(abs(exp_bias))], "_", gear[which.min(abs(exp_bias))], "-", occasions[which.min(abs(exp_bias))]),
           min_exp_rel_bias=min(exp_rel_bias, na.rm=TRUE),
           min_exprb_id=paste0(samp_type[which.min(exp_rel_bias)], "_", pop_id[which.min(exp_rel_bias)], "-", catch_id[which.min(exp_rel_bias)], "_", gear[which.min(exp_rel_bias)], "-", occasions[which.min(exp_rel_bias)]),
           max_exp_rel_bias=max(exp_rel_bias, na.rm=TRUE),
           max_exprb_id=paste0(samp_type[which.max(exp_rel_bias)], "_", pop_id[which.max(exp_rel_bias)], "-", catch_id[which.max(exp_rel_bias)], "_", gear[which.max(exp_rel_bias)], "-", occasions[which.max(exp_rel_bias)]),
           min_rel_abs_exp_bias=min(abs(exp_rel_bias), na.rm=TRUE),
           min_abs_exprb_id=paste0(samp_type[which.min(abs(exp_rel_bias))], "_", pop_id[which.min(abs(exp_rel_bias))], "-", catch_id[which.min(abs(exp_rel_bias))], "_", gear[which.min(abs(exp_rel_bias))], "-", occasions[which.min(abs(exp_rel_bias))]),
           min_exp_prec=min(exp_precision, na.rm=TRUE),
           min_expp_id=paste0(samp_type[which.min(exp_precision)], "_", pop_id[which.min(exp_precision)], "-", catch_id[which.min(exp_precision)], "_", gear[which.min(exp_precision)], "-", occasions[which.min(exp_precision)]),
           max_exp_prec=max(exp_precision, na.rm=TRUE),
           max_expp_id=paste0(samp_type[which.max(exp_precision)], "_", pop_id[which.max(exp_precision)], "-", catch_id[which.max(exp_precision)], "_", gear[which.max(exp_precision)], "-", occasions[which.max(exp_precision)]),
           min_exp_sd=min(exp_precision*abs(exp_trnd), na.rm=TRUE),
           min_expsd_id=paste0(samp_type[which.min(exp_precision*abs(exp_trnd))], "_", pop_id[which.min(exp_precision*abs(exp_trnd))], "-", catch_id[which.min(exp_precision*abs(exp_trnd))], "_", gear[which.min(exp_precision*abs(exp_trnd))], "-", occasions[which.min(exp_precision*abs(exp_trnd))]),
           max_exp_sd=max(exp_precision*abs(exp_trnd), na.rm=TRUE),
           max_expsd_id=paste0(samp_type[which.max(exp_precision*abs(exp_trnd))], "_", pop_id[which.max(exp_precision*abs(exp_trnd))], "-", catch_id[which.max(exp_precision*abs(exp_trnd))], "_", gear[which.max(exp_precision*abs(exp_trnd))], "-", occasions[which.max(exp_precision*abs(exp_trnd))]),
           min_rely=min(reliability),
           max_rely=max(reliability))
saveRDS(tbl, "D:/_output/4-tables/Worst_Best/trnd_min_max_by_samp_type_and_estimator.rds")

values<-data.frame(min_bias=min(dat$bias, na.rm=TRUE),
                   max_bias=max(dat$bias, na.rm=TRUE),
                   min_abs_bias=min(abs(dat$bias), na.rm=TRUE),
                   min_prec=min(dat$precision, na.rm=TRUE),
                   max_prec=max(dat$precision, na.rm=TRUE),
                   min_exp_bias=min(dat$exp_bias, na.rm=TRUE),
                   max_exp_bias=max(dat$exp_bias, na.rm=TRUE),
                   min_abs_exp_bias=min(abs(dat$exp_bias), na.rm=TRUE),
                   min_rel_exp_bias=min(dat$exp_rel_bias, na.rm=TRUE),
                   max_rel_exp_bias=max(dat$exp_rel_bias, na.rm=TRUE),
                   min_rel_abs_exp_bias=min(abs(dat$exp_rel_bias), na.rm=TRUE),
                   min_exp_prec=min(dat$exp_precision, na.rm=TRUE),
                   max_exp_prec=max(dat$exp_precision, na.rm=TRUE),
                   min_rely=min(dat$reliability),
                   max_rely=max(dat$reliability),
                   min_b_id=paste0(tbl$estimator[which.min(tbl$min_bias)],"_",tbl$min_b_id[which.min(tbl$min_bias)]),
                   max_b_id=paste0(tbl$estimator[which.max(tbl$max_bias)],"_",tbl$max_b_id[which.max(tbl$max_bias)]),
                   min_abs_b_id=paste0(tbl$estimator[which.min(tbl$min_abs_bias)],"_",tbl$min_abs_b_id[which.min(tbl$min_abs_bias)]),
                   min_p_id=paste0(tbl$estimator[which.min(tbl$min_prec)],"_",tbl$min_p_id[which.min(tbl$min_prec)]),
                   max_p_id=paste0(tbl$estimator[which.max(tbl$max_prec)],"_",tbl$max_p_id[which.max(tbl$max_prec)]),
                   min_sd_id=paste0(tbl$estimator[which.min(tbl$min_sd)],"_",tbl$min_sd_id[which.min(tbl$min_sd)]),
                   max_sd_id=paste0(tbl$estimator[which.max(tbl$max_sd)],"_",tbl$max_sd_id[which.max(tbl$max_sd)]),
                   min_expb_id=paste0(tbl$estimator[which.min(tbl$min_exp_bias)],"_",tbl$min_expb_id[which.min(tbl$min_exp_bias)]),
                   max_expb_id=paste0(tbl$estimator[which.max(tbl$max_exp_bias)],"_",tbl$max_expb_id[which.max(tbl$max_exp_bias)]),
                   min_abs_expb_id=paste0(tbl$estimator[which.min(tbl$min_abs_exp_bias)],"_",tbl$min_abs_expb_id[which.min(tbl$min_abs_exp_bias)]),
                   min_expp_id=paste0(tbl$estimator[which.min(tbl$min_exp_prec)],"_",tbl$min_expp_id[which.min(tbl$min_exp_prec)]),
                   max_expp_id=paste0(tbl$estimator[which.max(tbl$max_exp_prec)],"_",tbl$max_expp_id[which.max(tbl$max_exp_prec)]),
                   min_expsd_id=paste0(tbl$estimator[which.min(tbl$min_exp_sd)],"_",tbl$min_expsd_id[which.min(tbl$min_exp_sd)]),
                   max_expsd_id=paste0(tbl$estimator[which.max(tbl$max_exp_sd)],"_",tbl$max_expsd_id[which.max(tbl$max_exp_sd)]))
saveRDS(values, "D:/_output/4-tables/Worst_Best/SW_trnd_min_max.rds")
val_trnd<-readRDS("D:/_output/4-tables/Worst_Best/SW_trnd_min_max.rds")


### PLOT EXAMPLES
#### EXP BIAS
##### EXTREMELY LARGE NEGATIVE
        strsplit(as.character(values$min_expb_id), "_")
        estm<-paste0(strsplit(as.character(values$min_expb_id), "_")[[1]][1],"_", strsplit(as.character(values$min_expb_id), "_")[[1]][2])
        samp_t<-strsplit(as.character(values$min_expb_id), "_")[[1]][3]
        id<-strsplit(as.character(values$min_expb_id), "_")[[1]][4]
        pop_id<-as.numeric(strsplit(id, "-")[[1]][1])
        catch_id<-as.numeric(strsplit(id, "-")[[1]][2])
        gocc<-strsplit(as.character(values$min_expb_id), "_")[[1]][5]
        gear<-strsplit(gocc, "-")[[1]][1]
        occs<-strsplit(gocc, "-")[[1]][2]
        rm(id,gocc)
        info<-dat[which(dat$samp_type==samp_t & dat$pop_id==pop_id & dat$catch_id==catch_id 
                  & dat$gear==gear & dat$occasions==occs & dat$estimator==estm), ]
        c1<-info$exp_pop_trnd
        c2<-info$exp_trnd
        abund<-readRDS(paste0("D:/_output/1-populations/sim_pop_", pop_id, ".rds"))
        abund<-sum(abund$Z[,1])
        curve(abund*c2^(x-1), 1, 10, type="l", col="red", xlab="Year", ylab="Population Size", yaxt='n', main="Trend: Worst Negative Bias")
        curve(abund*c1^(x-1), 1, 10, type="l", col="black", add=TRUE)
        legend(7.5, 20000, legend=c("Actual", "Estimate"),
               col=c("black", "red"), lty=1, cex=0.8)

##### EXTREMELY LARGE POSITIVE
        strsplit(as.character(values$max_expb_id), "_")
        estm<-paste0(strsplit(as.character(values$max_expb_id), "_")[[1]][1],"_", strsplit(as.character(values$max_expb_id), "_")[[1]][2])
        samp_t<-strsplit(as.character(values$max_expb_id), "_")[[1]][3]
        id<-strsplit(as.character(values$max_expb_id), "_")[[1]][4]
        pop_id<-as.numeric(strsplit(id, "-")[[1]][1])
        catch_id<-as.numeric(strsplit(id, "-")[[1]][2])
        gocc<-strsplit(as.character(values$max_expb_id), "_")[[1]][5]
        gear<-strsplit(gocc, "-")[[1]][1]
        occs<-strsplit(gocc, "-")[[1]][2]
        rm(id,gocc)
        info<-dat[which(dat$samp_type==samp_t & dat$pop_id==pop_id & dat$catch_id==catch_id 
                        & dat$gear==gear & dat$occasions==occs & dat$estimator==estm), ]
        c1<-info$exp_pop_trnd
        c2<-info$exp_trnd
        abund<-readRDS(paste0("D:/_output/1-populations/sim_pop_", pop_id, ".rds"))
        abund<-sum(abund$Z[,1])
        curve(abund*c2^(x-1), 1, 10, type="l", col="red", xlab="Year", ylab="Population Size", yaxt='n', main="Trend: Worst Positive Bias")
        curve(abund*c1^(x-1), 1, 10, type="l", col="black", add=TRUE)
        legend(1, 3000000000, legend=c("Actual", "Estimate"),
               col=c("black", "red"), lty=1, cex=0.8)
        
##### EXTREMELY SMALL
        strsplit(as.character(values$min_abs_expb_id), "_")
        estm<-paste0(strsplit(as.character(values$min_abs_expb_id), "_")[[1]][1],"_", strsplit(as.character(values$min_abs_expb_id), "_")[[1]][2])
        samp_t<-strsplit(as.character(values$min_abs_expb_id), "_")[[1]][3]
        id<-strsplit(as.character(values$min_abs_expb_id), "_")[[1]][4]
        pop_id<-as.numeric(strsplit(id, "-")[[1]][1])
        catch_id<-as.numeric(strsplit(id, "-")[[1]][2])
        gocc<-strsplit(as.character(values$min_abs_expb_id), "_")[[1]][5]
        gear<-strsplit(gocc, "-")[[1]][1]
        occs<-strsplit(gocc, "-")[[1]][2]
        rm(id,gocc)
        info<-dat[which(dat$samp_type==samp_t & dat$pop_id==pop_id & dat$catch_id==catch_id 
                        & dat$gear==gear & dat$occasions==occs & dat$estimator==estm), ]
        c1<-info$exp_pop_trnd
        c2<-info$exp_trnd
        abund<-readRDS(paste0("D:/_output/1-populations/sim_pop_", pop_id, ".rds"))
        abund<-sum(abund$Z[,1])
        curve(abund*c2^(x-1), 1, 10, type="l", col="red", lwd=2, xlab="Year", ylab="Population Size", yaxt='n', main="Trend: Best Bias")
        curve(abund*c1^(x-1), 1, 10, type="l", col="black", add=TRUE)
        legend(7.5, 58000, legend=c("Actual", "Estimate"),
               col=c("black", "red"), lty=1, cex=0.8)
        
#### EXP PRECISION
##### WORST
        strsplit(as.character(values$max_expp_id), "_")
        estm<-paste0(strsplit(as.character(values$max_expp_id), "_")[[1]][1],"_", strsplit(as.character(values$max_expp_id), "_")[[1]][2])
        samp_t<-strsplit(as.character(values$max_expp_id), "_")[[1]][3]
        id<-strsplit(as.character(values$max_expp_id), "_")[[1]][4]
        pop_id<-as.numeric(strsplit(id, "-")[[1]][1])
        catch_id<-as.numeric(strsplit(id, "-")[[1]][2])
        gocc<-strsplit(as.character(values$max_expp_id), "_")[[1]][5]
        gear<-strsplit(gocc, "-")[[1]][1]
        occs<-strsplit(gocc, "-")[[1]][2]
        rm(id,gocc)
        info<-dat[which(dat$samp_type==samp_t & dat$pop_id==pop_id & dat$catch_id==catch_id 
                        & dat$gear==gear & dat$occasions==occs & dat$estimator==estm), ]
        cup<-info$pop_trnd+1.96*info$exp_precision
        clo<-info$pop_trnd-1.96*info$exp_precision
        abund<-readRDS(paste0("D:/_output/1-populations/sim_pop_", pop_id, ".rds"))
        abund<-sum(abund$Z[,1])
        curve(abund*exp(cup)^(x-1), 1, 5, ylim=c(0,10000000000), type="l", col="black", xlab="Year", ylab="Population Size", main="Trend: Worst Precision")
        curve(abund*exp(clo)^(x-1), 1, 5, type="l", col="black", add=TRUE)
        t<-seq(1,5,0.01)
        y1<-abund*exp(cup)^(t-1)
        y2<-abund*exp(clo)^(t-1)
        polygon(c(t, rev(t)), c(y1, rev(y2)), col = "grey", border=NA)
        curve(abund*exp(clo)^(x-1), 1, 5, type="l", col="black", add=TRUE)
        curve(abund*exp(cup)^(x-1), 1, 5, type="l", col="black", add=TRUE)

##### BEST
        strsplit(as.character(values$min_expp_id), "_")
        estm<-paste0(strsplit(as.character(values$min_expp_id), "_")[[1]][1],"_", strsplit(as.character(values$min_expp_id), "_")[[1]][2])
        samp_t<-strsplit(as.character(values$min_expp_id), "_")[[1]][3]
        id<-strsplit(as.character(values$min_expp_id), "_")[[1]][4]
        pop_id<-as.numeric(strsplit(id, "-")[[1]][1])
        catch_id<-as.numeric(strsplit(id, "-")[[1]][2])
        gocc<-strsplit(as.character(values$min_expp_id), "_")[[1]][5]
        gear<-strsplit(gocc, "-")[[1]][1]
        occs<-strsplit(gocc, "-")[[1]][2]
        rm(id,gocc)
        info<-dat[which(dat$samp_type==samp_t & dat$pop_id==pop_id & dat$catch_id==catch_id 
                        & dat$gear==gear & dat$occasions==occs & dat$estimator==estm), ]
        cup<-info$pop_trnd+1.96*info$exp_precision
        clo<-info$pop_trnd-1.96*info$exp_precision
        abund<-readRDS(paste0("D:/_output/1-populations/sim_pop_", pop_id, ".rds"))
        abund<-sum(abund$Z[,1])
        curve(abund*exp(cup)^(x-1), 1, 10,yaxt='n', ylim=c(30000,59600), type="l", col="black", xlab="Year", ylab="Population Size", main="Trend: Best Precision")
        curve(abund*exp(clo)^(x-1), 1, 10, type="l", col="black", add=TRUE)
        t<-seq(1,10,0.01)
        y1<-abund*exp(cup)^(t-1)
        y2<-abund*exp(clo)^(t-1)
        polygon(c(t, rev(t)), c(y1, rev(y2)), col = "grey", border=NA)
