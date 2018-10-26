###############################################
############# Find the model ##################
###############################################

library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$cat <- ifelse( d$category=="ME" , 1 , 0 )
d$tech <- ifelse( d$technique=="NT" , 1 , 0 )

mstan1 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat + bct*cat*tech,
    a ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bc ~ dnorm(0,10),
    bct ~ dnorm(0,10)
  ) ,
  data=d)
mstan2 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ) ,
  data=d)
mstan3 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech,
    a ~ dnorm(0,10),
    bt ~ dnorm(0,10)
  ) ,
  data=d)
mstan4 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bc*cat,
    a ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ) ,
  data=d)
compare(mstan1, mstan2, mstan3, mstan4)

###############################################
########### find good PRIORS ALPHA ############
###############################################

library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$cat <- ifelse( d$category=="ME" , 1 , 0 )
d$tech <- ifelse( d$technique=="NT" , 1 , 0 )

mstan1 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,1),
    bt ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ) ,
  data=d)
mstan2 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,5),
    bt ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ) ,
  data=d)
mstan3 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ) ,
  data=d)
mstan4 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,50),
    bt ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ) ,
  data=d)

precis(mstan1)
precis(mstan2)
precis(mstan3)
precis(mstan4)

compare(mstan1, mstan2, mstan3, mstan4)

##############################################
########## find good PRIORS BETA #############
##############################################

library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$cat <- ifelse( d$category=="ME" , 1 , 0 )
d$tech <- ifelse( d$technique=="NT" , 1 , 0 )

mstan1 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,1),
    bt ~ dnorm(0,5),
    bc ~ dnorm(0,5)
  ) ,
  data=d)
mstan2 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,7),
    bt ~ dnorm(0,7),
    bc ~ dnorm(0,7)
  ) ,
  data=d)
mstan3 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,1),
    bt ~ dnorm(0,8),
    bc ~ dnorm(0,8)
  ) ,
  data=d)
mstan4 <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,1),
    bt ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ) ,
  data=d)

precis(mstan1)
precis(mstan2)
precis(mstan3)
precis(mstan4)

compare(mstan1, mstan2, mstan3, mstan4)

###############################################
################# Best Model ##################
###############################################

library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$cat <- ifelse( d$category=="ME" , 1 , 0 )
d$tech <- ifelse( d$technique=="NT" , 1 , 0 )

mstan <- map2stan(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,1),
    bt ~ dnorm(0,1),
    bc ~ dnorm(0,1)
  ) ,
  data=d)

post <- extract.samples(mstan)
str(post)
plot(mstan)
precis(mstan)


################ Others

#### Sampling again in paralelles
mstan_4chains <- map2stan( mstan , chains=4 , cores=4 )
precis(mstan_4chains)
plot(mstan_4chains)

## R code 8.8
post <- extract.samples( mstan )
str(post)

## R code 8.9
#pairs(mstan)
#pairs(post)

plot(mstan)