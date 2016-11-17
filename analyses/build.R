
setwd('C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/analyses')


# RENDER SITE
rmarkdown::render_site()# build website
dir()

junk <- c(dir(path=getwd(), pattern=c("inp")),
    dir(path=getwd(), pattern=c("vcv")),
    dir(path=getwd(), pattern=c("out")),
    dir(path=getwd(), pattern=c("res")))
if(length(junk)>0){file.remove(junk) }





# COPY FILES TO DOCS FOR GITHUB.IO
system(paste("xcopy", 
    '"C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/analyses/_site"', 
    '"C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/Docs"',
    "/E /C /H /R /K /O /Y")) 
    
    
    