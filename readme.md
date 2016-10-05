 <script type="text/javascript"
      src="https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
    </script>

# Abundance
##Site-level
* How many replicates are needed to estimate abundance reliably?  What is reliable?
    * Constant p
    * Heterogeneous capture probability
    * Some movement in and out…
* Benefit of using tagged critters?

# Segment-level

* Estimates from 25% of bends in segment per year?
* Dependent on bend size
* Relate to density?
* Consequences of some movement?

# Survival

* Robust design
    * M0- varies by bend  
    * Mt – varies by bend and secondary occasion   
    * Constant phi within segment, varies among years 
	    * ![equation](http://latex.codecogs.com/gif.latex?logit(\phi) = \Beta_0 + \epsilon_{year}) where $\epsilon_{year} \sim N(0,\sigma)$
* Robust design with telemetry 
    * In segment (perfect detection, imperfect detection)
    * River sweep (perfect detection, imperfect detection)
	    * $logit(\phi) = \Beta_0 + \epsilon_{year} where $\epsilon_{year} \sim N(0,\sigma)$
		* $Z_{track,segment,year}\sim binomial(\phi_{segment,year})
		* River sweep to inform $\phi$,$\gamma\prime$ and $\gamma\prime\prime$ from tracking tags

1) Do fish move large distances outside of migrations?  What is the optimal time to do capture-recapture? Options: none, some, a lot.  

2) What is better to estimate as a random subset of bends or randomly tag a bunch of fish and recapture in random bends?  
http://latex.codecogs.com/gif.latex?\phi

		
