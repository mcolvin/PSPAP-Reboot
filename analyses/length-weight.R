
n=1000
Len<- runif(n, 100,2000)
Size<- factor(ifelse(Len<1000, "Small","Big"),levels=c("Small","Big"))
Weight<-ifelse(Size=="Small", 10^-5.1*Len^3.2, 10^-5.6*Len^3.4)
Weight<- rlnorm(n,log(Weight), 0.2)
dat<- data.frame(Len=Len, Size=Size, Weight=Weight)

plot(Weight~Len,dat)

fit<- lm(log10(Weight)~Size+log10(Len)+ Size:log10(Len),dat)

summary(fit)

# PREDICTED WEIGHTS
dat$pred<- 10^(predict(fit,dat))

plot(Weight~Len,dat)
points(pred~Len,dat, col="blue")# you can see the break at 1000

# Coefficients
# small fish
## a'
coef(fit)["(Intercept)"] # close to -5.01
## b
coef(fit)["log10(Len)"] # close to 3.2


# big fish: these numbers are close, but bounce around because there 
# are not as many of them as the smaller ones.  
## a'
coef(fit)["(Intercept)"]+coef(fit)["SizeBig"] # close to -5.6
## b
coef(fit)["log10(Len)"] + coef(fit)["SizeBig:log10(Len)"] # close to -3.2



