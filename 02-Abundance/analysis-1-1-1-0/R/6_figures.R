figures<-function(n,...)
	{
    add<- list(...)
	if(n==1)
		{
        pdat<-tables("f1")	
        
		par(mfrow=c(5,6),mar=c(0,0,0,0),oma=c(3,5,3,1),las=1,cex.axis=0.8)
		plot_metrics(metric="reliability",NN=add$NN,xxt="n",main=TRUE,dat=pdat)
		plot_metrics(metric="accuracy_N",NN=add$NN,xxt="n",dat=pdat)
		plot_metrics(metric=" precision_N",NN=add$NN,xxt="n",dat=pdat)
		plot_metrics(metric=" accuracy_p",NN=add$NN,xxt="n",dat=pdat)
		plot_metrics(metric=" precision_p",NN=add$NN,xxt="s",dat=pdat)
        mtext(side=1, "Occasions",outer=TRUE,line=2)
        mtext(side=2, "Metric",outer=TRUE,line=2,las=3) 
        }
    
        
        

	}