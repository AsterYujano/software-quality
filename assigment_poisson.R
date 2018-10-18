library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$cat <- ifelse( d$category=="LE" , 1 , 0 )
d$tech <- ifelse( d$technique=="OT" , 1 , 0 )

d$log_pop <- log(d$population)
d$contact_high <- ifelse( d$contact=="high" , 1 , 0 )

#first model
mpoisson_interactions <- map(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bc*cat + bt*tech + bct*cat*tech,
    a ~ dnorm(0,100),
    c(bc,bt,bct) ~ dnorm(0,1)
  ),
  data=d )
precis(mpoisson_interactions,corr=TRUE)
plot(precis(mpoisson_interactions))

#Second model
mpoisson_NoInterac <- map(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bc*cat + bt*tech,
    a ~ dnorm(0,100),
    c(bc,bt) ~ dnorm(0,1)
  ),
  data=d )
precis(mpoisson_NoInterac,corr=TRUE)
plot(precis(mpoisson_NoInterac))

#Third model
mpoisson_cat <- map(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bc*cat,
    a ~ dnorm(0,100),
    c(bc) ~ dnorm(0,1)
  ),
  data=d )
precis(mpoisson_cat,corr=TRUE)
plot(precis(mpoisson_cat))

#Fourth model
mpoisson_tech <- map(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bc*cat,
    a ~ dnorm(0,100),
    c(bc) ~ dnorm(0,1)
  ),
  data=d )
precis(mpoisson_tech,corr=TRUE)
plot(precis(mpoisson_tech))

compare(mpoisson_interactions, mpoisson_NoInterac, mpoisson_cat, mpoisson_tech)
pairs(mpoisson_NoInterac)
