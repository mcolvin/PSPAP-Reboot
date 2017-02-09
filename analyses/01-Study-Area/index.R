
    source("./R/1-global.R")
    source("./R/2-functions.R")
    source("./R/3-load.R")
    source("./R/4-clean.R")
    source("./R/5-figures.R")
    source("./R/6-tables.R")
    source("./R/7-analysis.R")

    figures(1);fn<-"study-area-1.png"
    savePlot(paste0(getwd(),"/figures/",fn),type='png')

    dev.off()
    # HATCHERIES
    figures(3);fn<-"study-area-hatcheries.png"
    savePlot(paste0(getwd(),"/figures/",fn),type='png')
    dev.off()
       