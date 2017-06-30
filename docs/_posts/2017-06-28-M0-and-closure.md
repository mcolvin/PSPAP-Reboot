---
layout: post
title: What happens when a population is not closed? Part I
published: TRUE
bibliography: PSref.bib
csl: ecology.csl
---
<!-- Knit to HTML or MD
rmarkdown::render("2017-06-28-M0-and-closure.Rmd")

filename0<- "2017-06-28-M0-and-closure.Rmd"
filename<- "2017-06-28-M0-and-closure.md"

# Draft blog post:
docs<- "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/docs/_drafts/"
knitr::knit(filename,output=paste(docs,filename,sep=""))

# Final blog post
docs<- "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/docs/_posts/"
knitr::knit(filename0,output=paste(docs,filename,sep=""))
-->  
  
## In a nutshell

Here are the highlights for those that do not want to read the post
in its entirety.

1. Looking at some recent telemetry data it was apparent that 
Pallid Sturgeon move around from 1 day to the next, a least in the spring.
2. This violates the assumption of closure during secondary periods in 
a robust design estimator and the consequences of violating this assumption
are not transparent. 
3. To evaluate the effect of movement on abundance estimates we used a
simulation study to fit a closed population capture recapture abundance
estimator to a virtual population with known population size and capture
probability. 
4. We then allowed Pallid Sturgeon to move in and out of the study area with
a rates of 0 to 0.35 by increments of 0.05. 
5. Turns out that with movement in and out of the study area results has
a positive bias on abundance estimates. 
6. This raises some implications for the PSPAP V2.0 as sampling at larger
spatial scales (e.g., segment level) minimizes the effect of movement but 
comes at a trade off of potentially losing bend-level information. But the
trade offs can be formally evaluated using objectives network we put together
and the utilities used to value the fundamental objectives.


## Background

In thinking about the bend level as the sampling unit for the PSPAP one
of the designs, as well as a design that has been used in the past was
a Robust Design. One plan is to continue to use bends as the sampling unit
and revisit them over the course of a couple of days. However to estimate
the abundance at the bend level we need to assume closure for capture
recapture estimates, which might be violated as fish move around from 
day to day.  

## Approach

We will simulate a virtual population and capture process to evaluate 
the potential consequences of Pallid Sturgeon using a closed population 
estimator, the M0 model (Otis 1978). The M0 model is one of the simplest 
capture recapture estimators and it assumes that capture probability is 
the same among all capture occasions. 

There needs to be 2 or more capture occasions for this estimator to 
work. Suppose for this evaluation we use 3 occasions and that a sampling 
crew is using trotlines set overnight. So this effort would consist of a 
crew setting trotlines, checking gear after 24 hours and marking fish 
(occasion 1), reseting the lines, returning after 24 hours to check gear 
and mark fish (occasion 2) and this process continues until 3 occasions 
are reached. With a capture probability of 0.3 and no fish moving out or 
into the bend during the 3 days it takes to achieve the 3 capture 
occasions. 

 



| Capture history | Relative frequency | Expected frequency |
|:---------------:|:------------------:|:------------------:|
|       000       |       0.512        |        51.2        |
|       100       |       0.128        |        12.8        |
|       010       |       0.128        |        12.8        |
|       110       |       0.032        |        3.2         |
|       001       |       0.128        |        12.8        |
|       101       |       0.032        |        3.2         |
|       011       |       0.032        |        3.2         |
|       111       |       0.008        |        0.8         |

Given the capture probability ($p=0.2$), 3 capture occasions, and that 
there are 100 Pallid Sturgeon on the bend we would expect not to 
capture 51 fish (i.e., capture history = 000). If we are lucky we may 
capture 1 fish all 3 times, the probability of this is 0.008 in the 
table above ($p\cdot p \cdot p$). 

The expected captures make several assumptions about population
closure. Specifically, the fish contained in the study area:
 
1. Stay put (Emigration = 0),
2. New fish do not enter the bend (Immigration = 0), 
3. New fish are not recruited (Recruitment = 0), and 
4. Fish do not die (Survival = 1)

We also assume:

1. Fish do not lose tags,
2. We detect tags perfectly, and
3. Capture probability is equal among fish. 


In real life the outcome of sampling a bend over 3 occasions, would look 
like the table below if the assumptions were not violated. 




| Capture history | Number captured |
|:---------------:|:---------------:|
|       100       |       11        |
|       010       |       12        |
|       110       |        4        |
|       001       |        8        |
|       101       |        4        |
|       011       |        4        |



The population estimate from the M0 model given those capture frequencies
was 75.4 and the 95% confidence interval of 54.9-121.3.
Not too bad, the estimate was a bit lower than 100, but estimates are expected to b
be above and below the true value. Let's demonstrate this by running the same
simulation as above but with many replicate datasets, 1000 for this simulation. 




![]({{ site.baseurl }}/images/figure/unnamed-chunk-5-1.png)

The plot above illustrates the distribution of estimates and the red 
vertical line denotes the true abundance.  We need to look at this plot
on log scale because estimates are lognormally distributed. 
 Ok, we have some confidence that
our estimator works. Now we can build on this approach to evaluate what happens
if Pallid Sturgeon are moving in and out of a bend. 

We can specify random movement as the immigration rate being equal to 
the emigration rate. Emigration works by moving fish out of the bend but 
we need a population of fish to immigrate into the population. For 
example, if the emigration rate was 0.1 we would expect 10 of the 100 
Pallid Sturgeon in the bend to leave. There also needs to be Pallid 
Sturgeon to move into the bend. In this simulation there are 75 Pallid 
Sturgeon that can potentially immigrate into the bend. Given an 
emigration rate of 0.1 we can expect 7.5 Pallid Sturgeon to move into 
the bend.  Let's set the daily emigration and immigration rate from 0 to 0.35 
in increments of 0.05 and 
allow random movement in and out of the bend each day. 



The figure below illustrates the effect of movement of fish in and out 
of the bend between occasions on abundance estimates. Specifically, we 
see that with no migration, abundance estimates were unbiased. Abundance 
estimates increase with increasing movement of fish in and out of the 
bend. 



![]({{ site.baseurl }}/images/figure/unnamed-chunk-7-1.png)


The overestimate of abundance is because the capture probability is 
underestimated. Why is that? Well, consider instances where a marked fish moved out
of the bend and was not available for recapture or an unmarked fish
moves into the bend after occasion 1. The estimator assumes these fish
were in the bend for every sampling occasion and vulnerable to capture.
Since that is not the case but the model assumes it is, the capture probability 
estimate is negatively biased as you can see below and therefore the abundance estimates are 
positively biased.

## Coming up

Next we will look at how the Robust Design fares to violoating this assumption.
Stay tuned.


# References

Otis, D. L., K. P. Burnham, G. C. White, and D. R. Anderson. 1978. Statistical-inference from capture data on closed animal populations. Wildlife Monographs:7–135.

