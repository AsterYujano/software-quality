library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$tech <- ifelse( d$technique=="OT" , 1 , 0 )
d$cat <- ifelse( d$category=="LE" , 1 , 0 )

mbinom <- map2stan(
  alist(
    tp ~ dbinom( category , p ) ,
    logit(p) <- a + bt*tech ,
    a ~ dnorm(0,10) ,
    bt ~ dnorm(0,10)
  ) ,
  data=d )

############# example below
library(rethinking)
data(UCBadmit)
d <- UCBadmit
d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
m10.6 <- map(
  alist(
    admit ~ dbinom( applications , p ) ,
    logit(p) <- a + bm*male ,
    a ~ dnorm(0,10) ,
    bm ~ dnorm(0,10)
  ) ,
  data=d )
m10.7 <- map(
  alist(
    admit ~ dbinom( applications , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ) ,
  data=d )

## R code 10.24
plot(compare( m10.6 , m10.7 ))