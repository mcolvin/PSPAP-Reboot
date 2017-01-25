figures<-function(n)
	{
	if(n==1)
		{
        pdat<-tables("f1")	
        
		par(mfrow=c(5,6),mar=c(0,0,0,0),oma=c(3,3,3,1),las=1,cex.axis=0.8)
		plot_metrics(metric="reliability",NN=25,xxt="n",main=TRUE)
		plot_metrics(metric="accuracy_N",NN=25,xxt="n")
		plot_metrics(metric=" precision_N",NN=25,xxt="n")
		plot_metrics(metric=" accuracy_p",NN=25,xxt="n")
		plot_metrics(metric=" precision_p",NN=25,xxt="s")
        mtext(side=1, "Occasions",outer=TRUE,line=2)
        
        
		par(mfrow=c(5,6),mar=c(0,0,0,0),oma=c(3,3,3,1),las=1,usr=xx)
		plot_metrics(metric="reliability",NN=578,xxt="n")
		plot_metrics(metric="accuracy_N",NN=578,xxt="n")
		plot_metrics(metric=" precision_N",NN=578,xxt="n")
		plot_metrics(metric=" accuracy_p",NN=578,xxt="n")
		plot_metrics(metric=" precision_p",NN=578,xxt="s")		
		}
	}