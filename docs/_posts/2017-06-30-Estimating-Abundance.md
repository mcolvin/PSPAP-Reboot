---
layout: post
title: Estimating abundance using capture-recapture
published: false
---

<!-- 

for blog post:
docs<- "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/docs/_posts/"
filename<- "2017-06-30-Estimating-Abundance.md"
knit("index.Rmd", 
    ouput=paste(docs,filename,sep="")

To do:
1. set up rep tracking, sim returns 2 outputs, pit and pit+acoustic for bdn

rmarkdown::render("index.Rmd")# render index for this analysis
# the results of this analysis (i.e., figures, tables) are sourced by the gitbook

-->



## Site-level

* How many replicates are needed to estimate abundance reliably? What is 
reliable? 
    * Constant p
    * Heterogeneous capture probability
    * Some movement in and out
* Benefit of using tagged critters?


## Segment-level

* Estimates from 25% of bends in segment per year?
* Dependent on bend size
* Relate to density?
* Consequences of some movement?

## Major questions 

Each major question corresponds to a chapter in this report.

Chapter 3. How many occasions are needed to estimate abundance at the 
bend-level and segment level? 
Chapter 4.. How should segment estimates be aggregated? 
    1. Derived from average bend estimates?
    2. Estimated at the segment level from bend-level data?
3. Do acoustic tags help in reducing the number of occasions? 
    1. If so how many tags are needed? 
    2. Can segment sweeps help in estimating segment level abundance?
4. What is the value of using shovelnose to estimate a common capture 
probabilities? 

### Analysis objectives

The objectives of this analysis were to:

1. evaluate the benefit of using telemetry information in abundance estimates
2. if so, then how many telemetry fish and occasions are needed to estimate abundance given accurancy and precision criteria.



Randomly get fish within segment
probably better to just break out to bends and run that way.


## Methods

### Simulated population

* Segment assigned 25  578 1131 1683 2236 2789 3342 3894 4447 5000
* Number of telemetry fish was c(2,5,10,20,30)
* Number of occasions was 2,3,4, or 5
* Detection probabilities were 0.050, 0.125, 0.200, 0.275, 0.350, 0.425, and 0.500
* All possible combinations: 280
* 100 replicates of each combination


### Abundance estimator

#### Model

#### Using telemetry fish

#### Assumptions

* No movement
* No mortality
* No tag loss
* Equally vulnerable to capture



### Design criteria

The design should meet the following criteria:
1. Be estimable for 100% of simulations 
2. Estimates mean absolute deviation (MAD) within 20% of true value
3. Mean relative standard error (RSE) of estimate less than or equal to 20%
    1. Abundance

Segment level estimate
1. closed w/in bend (rd) N = sum(N_bend/bend length)
2. closed w/in segment  
3. 

### Decision model

#### Parameterization


#### Effort and Cost

* Trotlining (3 person crew)
    * 1 lead; 35 to 50 k/year 
    * 2 technicians ; 22 to 35 k/year
    * 35% fringe
* VR100 @ 2000 x 2
* Telemetry Tags @ 200 to 300 $/tag
* PIT Tags @ 1 $/tag




## Results

## Discussion
Rules of thumb, N recaptures? number of fish? 
use shovelnose for capture probabilities?




<!--NN: 25 578 1131 1683 2236 2789 3342 3894 4447 5000 -->
        



_Figure 1._
Figure of reliability, accuracy, and precision for abundance and detection probability estimates.
Abundance estimate reliability (top), accuracy (2nd from top), precision (middle row).
Bottom 2 rows present accuracy and precision for detection probability estimates.




# Analysis objectives

1. Evaluate the number of recapture occasions need to estimate capture probability($p$) and abundance ($N$) for
varying population and capture probabilities
2. Evaluate the use of tagged telemetry fish to reduce effort


