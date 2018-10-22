library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$cat <- ifelse( d$category=="LE" , 1 , 0 )
d$tech <- ifelse( d$technique=="OT" , 1 , 0 )

mb1 <- map(
  alist(
    tp ~ dbinom( 2, p ),
    logit(p) <- a + bt*tech,
    a ~ dnorm(0,10),
    c(bt) ~ dnorm(0,10)
  ),
  data=d )