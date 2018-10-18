library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$tech <- ifelse( d$technique=="OT" , 1 , 0 )
d$cat <- ifelse( d$category=="LE" , 1 , 0 )

mnorm_cat <- map(
  alist(
    tp ~ dnorm( mu, sigma),
    mu <- a + bc*cat,
    a ~ dnorm(0,100),
    bc ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(mnorm_cat)


mnorm_tech <- map(
  alist(
    tp ~ dnorm( mu, sigma),
    mu <- a + bt*tech,
    a ~ dnorm(0,100),
    bt ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(mnorm_tech)

mnorm_both <- map(
  alist(
    tp ~ dnorm( mu, sigma),
    mu <- a + bc*cat +bt*tech,
    a ~ dnorm(0,100),
    bc ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(mnorm_both)

compare(mnorm_cat, mnorm_tech, mnorm_both)
