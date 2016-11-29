figures<- function(n)
    {
    if(n==1)
        {# FIGURE 1
        # COMBINGING INFORMATION 
        x<- seq(-20,0,0.01)
        B0<- -12.5
        uncert<- 0.2
        p1<-dnorm(x,B0, uncert)
        p1<- p1/sum(p1)

        B0<- -11.5
        uncert<- 0.1
        p2<-dnorm(x,B0, uncert)
        p2<- p2/sum(p2)

        # BAYES THEORUM
        post<- p1*p2/sum(p1*p2)

        presPlot()

        matplot(plogis(x),cbind(p1,p2,post),type='l',xlim=c(0, 0.00002),las=1,
            xlab="Survival", ylab="",lwd=3, col=c("black", "black","grey"),lty=c(1,2,1))
        mtext(side=2, "Probability",line=0.25,cex=1.5,outer=TRUE)
        legend("topright", legend=c("Expert elicitation", "Level 1 exp.", "Assimilated"),col=c("black", "black","grey"),lty=c(1,2,1))
        }
    if(n==2)
        {
        # UNCERTINATY

        X<- seq(0,2,by=0.1)
        B0<- 0.1
        B1<- 2.2
        er<- 0.6
        y<- B0+B1*X
        lci<- y-1.96*er
        uci<- y+1.96*er
        p<- plogis(y)
        plot(X,p,lwd=2, lty=1,type='l',las=1,xlab="Condition",ylab="Survival")
        points(X,plogis(uci),type='l',lty=2)
        points(X,plogis(lci),type='l',lty=2)


        er<- rnorm(9000000,0, uncert)
        rw<- B0+er
        expert<- plogis(rw)

        bins<- seq(0, 0.00002,length=50)
        tmp<- findInterval(expert,bins)

        tmp<- as.data.frame(prop.table(table(tmp)))
        tmp<- merge(data.frame(tmp=c(1:length(bins))), tmp,all.x=TRUE)
        tmp[is.na(tmp)]<-0
        out<- tmp






        # Informative prior using MOM
        B0<--9.1
        uncert<- 0.5
        er<- rnorm(20000,0, uncert)
        expert<- plogis(B0+er)
        beta.parms <- beta.mom(mean(expert), sd(expert))
        a <- beta.parms[1]
        b <- beta.parms[2]
        a;b
        prior <- dbeta(p, a, b)



        B0<--8.5
        uncert<- 0.25
        er<- rnorm(20000,0, uncert)
        expert<- plogis(B0+er)
        beta.parms <- beta.mom(mean(expert), sd(expert))
        a_post <- beta.parms[1]
        b_post <- beta.parms[2]
        a_post;b_post
        p_post <- seq(0, 0.002, 0.00000001)
        post <- dbeta(p, a_post, b_post)

        plot(p, prior, type = "l", col = "black",
            xlab="Survival")
        points(p, post, type = "l", col = "red")
        post <- dbeta(p, a+a_post, b+b_post-a_post)
        points(p, post, type = "l", col = "blue")
        }
    }
