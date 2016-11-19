


# LOAD ANALYSIS OUTPUT

fn<-dir("./RD-Rmark/output")

if( "derived.RDS" %in% fn)
    {
    derived<- readRDS("./RD-Rmark/output/derived.RDS")
    }
