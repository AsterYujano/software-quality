library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")

d$lessexp <- ifelse( d$category=="LE" , 1 , 0 )
mexp <- map(
  alist(
    tp ~ dnorm( mu, sigma),
    mu <- a + bl*lessexp,
    a ~ dnorm(0,100),
    bl ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(mexp)

d$oldtech <- ifelse( d$technique=="OT" , 1 , 0 )
mtech <- map(
  alist(
    tp ~ dnorm( mu, sigma),
    mu <- a + bo*oldtech,
    a ~ dnorm(0,100),
    bo ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(mtech)
