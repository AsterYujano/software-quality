library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$tech <- ifelse( d$technique=="OT" , 1 , 0 )
d$cat <- ifelse( d$category=="LE" , 1 , 0 )
d$success <- ifelse(d$tp<10, 1, 0)

mbinom_both <- map(
  alist(
    success ~ dbinom(1, p),
    logit(p) <- a + bt*tech + bc*cat,
    a ~ dnorm(0,10),
    bt~ dnorm(0,10),
    bc ~ dnorm(0,10)
    ), data=d
)
mbinom_cat <- map(
  alist(
    success ~ dbinom(1, p),
    logit(p) <- a + bc*cat,
    a ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ), data=d
)
mbinom_tech <- map(
  alist(
    success ~ dbinom(1, p),
    logit(p) <- a + bt*tech,
    a ~ dnorm(0,10),
    bt~ dnorm(0,10)
  ), data=d
)
compare(mbinom_both, mbinom_cat, mbinom_tech)
plot(compare(mbinom_both, mbinom_cat, mbinom_tech))

#### TRY final comparaison

mpoisson_NoInterac <- map(
  alist(
    tp ~ dpois( lambda ),
    log(lambda) <- a + bc*cat + bt*tech,
    a ~ dnorm(0,100),
    c(bc,bt) ~ dnorm(0,1)
  ),
  data=d )

mnorm_both <- map(
  alist(
    tp ~ dnorm( mu, sigma),
    mu <- a + bc*cat +bt*tech,
    a ~ dnorm(0,100),
    bc ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )

mbinom_cat <- map(
  alist(
    success ~ dbinom(1, p),
    logit(p) <- a + bc*cat,
    a ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ), data=d
)

compare(mbinom_cat, mnorm_both, mpoisson_NoInterac)

#### Different priors

mbinom_cat1 <- map(
  alist(
    success ~ map2stan(1, p),
    logit(p) <- a + bc*cat,
    a ~ dnorm(0,10),
    bc ~ dnorm(0,5)
  ), data=d
)
mbinom_cat2 <- map(
  alist(
    success ~ dbinom(1, p),
    logit(p) <- a + bc*cat,
    a ~ dnorm(0,10),
    bc ~ dnorm(0,10)
  ), data=d
)
mbinom_cat3 <- map2stan(
  alist(
    success ~ dbinom(1, p),
    logit(p) <- a + bc*cat,
    a ~ dnorm(0,10),
    bc ~ dnorm(0,1)
  ), data=d
)

compare(mbinom_cat1, mbinom_cat2, mbinom_cat3)
stan(mbinom_cat3)

#### ESSAI 1 : Crash with R recompilation
library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$tech <- ifelse( d$technique=="OT" , 1 , 0 )
d$cat <- ifelse( d$category=="LE" , 1 , 0 )

mbinom.tech <- map2stan(
  alist(
    tp ~ dbinom( 1 , p ) ,
    logit(p) <- a + bt*tech ,
    a ~ dnorm(0,10),
    bt ~ dnorm(0,10)
  ) ,
  data=d, chains=2)

mbinom.cat <- map2stan(
  alist(
    tp ~ dbinom( 1 , p ) ,
    logit(p) <- a + bc*cat ,
    a ~ dnorm(0,10) ,
    bc ~ dnorm(0,10)
  ) ,
  data=d, chains=2)

compare(mbinom.tech, mbinom.cat)
plot(compare(mbinom.tech, mbinom.cat))

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