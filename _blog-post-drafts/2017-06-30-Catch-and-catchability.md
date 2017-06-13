---
layout: post
title: Catch, Catchability, & Capture Probability
published: false
---

<!-- 
for blog post:
docs<- "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/docs/_posts/"
filename<- "2017-06-30-Catch-and-catchability.md"
knitr::knit("2017-06-30-Catch-and-catchability.Rmd", 
    output=paste(docs,filename,sep=""))
-->



The catch equation is 

$Catch = q\cdot f \cdot N$

where, 

* $Catch$ is the number of fish captured
* $q$ is the catchability coefficient
* $f$ is the the amount of effort, and
* $N$ is the number of fish available for capture.

The parameter $q$ has a couple of different meanings. Classically, it 
was the power of a gear to catch fish per unit effort and had the units 
$1/time$ such that it became dimensionless when multiplied by $f$. The 
catchability coefficient also can be interpreted as the probability of 
catching 1 fish per unit effort (Hubert and Fabrizio 2007). The 
difference between the 2 interpretations depends on how $q$ is being 
applied. If $q$ is applied continuously, it is an instantaneous rate, 
familiar from the Graham-Schaeffer formulations of biomass dynamics 
model where 



$\frac{dBiomass}{dt}=r\cdot (\frac{K-B}{K})-q\cdot f\cdot B$

where,
* $r$ is the intrinsic growth rate,
* $K$ is the carrying capacity,
* $B$ is the biomass, and
* $q$ and $f$ are as previously described.

The equation above can be solved by integration to calculate continuous
biomass dynamics. This formulation is commonly used for biomass
but could be used for abundance. A key assumption is that fish are 
being removed continuously over time. Alternatively, a discrete formulation
is used, sometimes for simplicity because integration is not needed to 
forecast dynamics and other times because effort and capture happen over
a short period of time. 

The discrete time formulation for $q$ is what is of interest for evaluating
PSPAP design programs. The catchability coefficient $q$ is rarely estimated
in fishery monitoring efforts. This is because it is often unknown what the 
population abundance that is vulnerable to the gear actually is. However, 
when $q$ is treated discretely it represents the probability of capturing 
1 fish per unit effort.  If you do the math on that, it turns out that 
capture probability ($p$), which is easily estimated by capture recapture, 
is: 

$p = q \cdot f$.

Let's prove this with a quick simulation in R.

Suppose we have 100 individual fish in a river bend and the gear 
specific catchability coefficient $q$ was $0.001^{-minute}$. Further 
suppose that a PSPAP crew fished that gear 50 minutes. We would expect
the catch to be $Catch = 0.001 \cdot 50 \cdot 100 = a\cdot f \cdot N$


```r
q<- 0.001
N<- 100
f<-50
Catch<- q*f*N
Catch 
```

```
## [1] 5
```

The catch was 5 fish, which if we calculate the capture probability as
5/100 it ends up being $p = 0.05 = \frac{5}{100} = q*f$.

Unfortunately we rarely know what $N$ truly is so we cannot estimate
$q$. But we can capture individual fish repeatedly to estimate $p$ using
mark recapture estimators which in turn means that for evaluating PSPAP 
designs, especially designs using relative abundance we can take advantage 
of previous work estimating capture probability for PSPAP gears.


# Capture probabilities of varying PSPAP gears

Several studies estimated capture probability of Pallid Sturgeon using
different gears (Table 1). Let's clear up some jargon first. 
Estimates in these studies were called capture probability and conditional
capture probability.  What is the difference?


![Sub-Adult Pallid Sturgeons]({{ site.baseurl }}/images/14979392701_a6e3667e14_o.jpg)

Table 

* Trammel nets

* Trot lines
* Otter trawls
* Gill nets

* All 4 gears
    * Pierce et al. found Recapture probability varied among year, did not
    evaluate difference among gears. If we can pull standard effort
    we can get q. $p$ in this study varied from 0.02 to 0.16 when allowed
    to vary and 0.08 (se = 0.02) when pooled. There was little evidence
    for p to vary among years so stick to 0.08 estimate.





 
