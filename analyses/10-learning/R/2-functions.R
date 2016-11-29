

beta.mom <- function(mean, sd) 
    {
    # FUNCTION TO CACLUATE BETA MOMEMNTS FROM MEAN AND SD
    v <- sd^2
    x <- mean
    a <- x * (x * (1 - x)/v - 1)
    b <- (1 - x) * (x * (1 - x)/v - 1)
    c(a, b)
    }