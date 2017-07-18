
library(rmarkdown)
library(knitr)
library(pander)
setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pradel-test")

render("pradel.Rmd")
RMark::cleanup(ask=FALSE) # clean up orpham mark files

knitr::pandoc('main.md', format='html')
