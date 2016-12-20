tables<-function(n)
	{
	if(n=="f1")# reliability, accuracy, and precision
		{
		out$N_hat<- ifelse(out$N_hat> 100*out$N,NA,out$N_hat)
		out$p_hat<- ifelse(out$p_hat> 100*out$p,NA,out$p_hat)
		tmp<-ddply(out,.(type,ntrack,p,N,nocc),summarize,
			reliability=length(na.omit(N_hat))/length(N_hat),
			accuracy_N = mean(abs(N_hat-N),na.rm=TRUE)/max(N),
			accuracy_p = mean(abs(p_hat-p),na.rm=TRUE)/max(p),
			precision_N = mean(N_hat_se/N_hat,na.rm=TRUE),
			precision_p = mean(p_hat_se/p_hat,na.rm=TRUE))

		return(tmp)
		}
	}
	