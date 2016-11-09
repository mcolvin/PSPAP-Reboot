
setwd('C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/analyses')
# RENDER SITE
rmarkdown::render_site()# build website
# COPY FILES TO DOCS FOR GITHUB.IO
system(paste("xcopy", 
    '"C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/analyses/_site"', 
    '"C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/Docs"',
    "/E /C /H /R /K /O /Y")) 
    
    
    