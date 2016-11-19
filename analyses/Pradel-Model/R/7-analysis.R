


```{r}
# SIMULATE POPULATION AND CAPTURE HISTORIES
n_occ<- 10
phi<- rep(0.8,n_occ-1)
p<- rep(0.3,n_occ)
f<- 0.195
n<- 2000 # initial population size
R<- c(n)
for(i in 2:n_occ)
	{
	R<- c(R,rpois(1,n[i-1]*f))
	n<- c(n, rbinom(1,n[i-1],phi)+R[i])	
	}
super_n<- sum(R) # total population
# WHEN DO CRITTERS RECRUIT TO THE POPULATION?
ent<-rep(c(1:length(R)),R)
ch<-Z<- matrix(0,super_n,n_occ)
for(i in 1:super_n)
	{
	Z[i,ent[i]]<-1
	indx<- ent[i]+1
	if(indx<=n_occ)
		{
		for(j in indx:n_occ)
			{
			Z[i,j]<- rbinom(1,1,phi[j-1]*Z[i,j-1])
			}
		}		
	}
```

```{r}
plot(colSums(Z),type='b',ylab="Abundance",xlab="Years")
```
```{r}	
# SIMULATE CAPTURE HISTORY
for(i in 1:super_n)
	{
	for(j in 1:n_occ)
		{
		ch[i,j]<- rbinom(1,1,p[j]*Z[i,j])
		}
	}
```

```{r}
ch<- ch[which(apply(ch,1,sum)!=0),]
```

```{r}
library(RMark)
# PROCESS CPATURE HISTORIES INTO A DATA.FRAME
ch<- data.frame(ch=apply(ch,1, paste,collapse=""),stringsAsFactors = FALSE)
# PROCESS CPATURE HISTORIES INTO A LIST FOR MARK
ch_proc<- process.data(ch,model="Pradrec", begin.time=2005)
# MAKE THE DESIGN MATRIX FOR THE CMR DATA
ddl<-make.design.data(ch_proc)
```

```{r}
Phi.dot=list(formula=~1)
p.dot=list(formula=~1)
f.dot=list(formula=~1)
m1<-mark(ch_proc,ddl,model.parameters=list(Phi=Phi.dot,p=p.dot,f=f.dot))
```

```{r}
ch_proc<- process.data(ch,model="Pradlambda", begin.time=2005)
ddl<-make.design.data(ch_proc)
Phi.dot=list(formula=~1)
p.dot=list(formula=~1)
Lambda.dot=list(formula=~1)
m1<-mark(ch_proc,ddl,model.parameters=list(Phi=Phi.dot,p=p.dot,Lambda=Lambda.dot))
```
`r round(exp(coef(m1)[3,1]),5)`
`r round(exp(coef(m1)[3,2:3]),5)`

```{r}
irc<- c(37,31,24,27,6,32,10,26,6)
# EFFECT OF IRC
B_irc<- 0.008
B_0<- -1.33
f<- exp(B_0 + B_irc*irc)
```


```{r}
plot(f~irc, ylab="Recruitment rate", xlab="IRC")
```

```{r}
# MODEL AS A FUNCTION OF IRC
n_occ<- 10
phi<- rep(0.8,n_occ-1)
p<- rep(0.3,n_occ)
n<- 2000 # initial population size
R<- c(n)
for(i in 2:n_occ)
	{
	R<- c(R,rpois(1,n[i-1]*f[i-1]))
	n<- c(n, rbinom(1,n[i-1],phi)+R[i])	
	}
super_n<- sum(R) # total population
# WHEN DO CRITTERS RECRUIT TO THE POPULATION?
ent<-rep(c(1:length(R)),R)
ch<-Z<- matrix(0,super_n,n_occ)
for(i in 1:super_n)
	{
	Z[i,ent[i]]<-1
	indx<- ent[i]+1
	if(indx<=n_occ)
		{
		for(j in indx:n_occ)
			{
			Z[i,j]<- rbinom(1,1,phi[j-1]*Z[i,j-1])
			}
		}		
	}
```


```{r}
plot(colSums(Z),type='b',ylab="Abundance", xlab="Occasion")
```

```{r}
# SIMULATE CAPTURE HISTORY
for(i in 1:super_n)
	{
	for(j in 1:n_occ)
		{
		ch[i,j]<- rbinom(1,1,p[j]*Z[i,j])
		}
	}
ch<- ch[which(apply(ch,1,sum)!=0),]

```


```{r}
ch<- data.frame(ch=apply(ch,1, paste,collapse=""),stringsAsFactors = FALSE)
ch_proc<- process.data(ch,model="Pradrec", begin.time=2005)
ddl<-make.design.data(ch_proc)
ddl$f$irc<- irc # ADD AMOUNT OF IRC TO USE AS A COVARIATE
```

```{r}
# FIT PRADEL RECRUITMENT MODEL WITH F A FUNCTION OF IRC
Phi.dot=list(formula=~1)
p.dot=list(formula=~1)
f.irc=list(formula=~irc+1)
m1<-mark(ch_proc,ddl,model.parameters=list(Phi=Phi.dot,p=p.dot,f=f.irc))
```


```{r}
f.dot=list(formula=~1)
f.irc=list(formula=~irc+1)
m1<-mark(ch_proc,ddl,model.parameters=list(Phi=Phi.dot,p=p.dot,f=f.dot))
m2<-mark(ch_proc,ddl,model.parameters=list(Phi=Phi.dot,p=p.dot,f=f.irc))

summary(m1)$AICc # AICc value for m1
summary(m2)$AICc # AICc value for m2
```


```{r}
ch_proc<- process.data(ch,model="Pradlambda", begin.time=2005)
ddl<-make.design.data(ch_proc)
ddl$Lambda$irc<- irc # ADD AMOUNT OF IRC TO USE AS A COVARIATE
Phi.dot=list(formula=~1)
p.dot=list(formula=~1)
Lambda.irc=list(formula=~irc+1)
m1<-mark(ch_proc,ddl,model.parameters=list(Phi=Phi.dot,p=p.dot,Lambda=Lambda.irc))
```
