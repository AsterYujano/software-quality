#########
## NEW ##
#########
library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
#etude <- d
#head(d)

##################
# Like lecture 5 (foxes) #
##################
# plot des notes de TP par la categorie auquel il appartient
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

#############
## Try 
######
library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
d$lessexp <- ifelse( d$category=="LE" , 1 , 0 )
d$oldtech <- ifelse( d$technique=="OT" , 1 , 0 )
d$recipient <- NULL
m10.4_map <- map(
  alist(
    oldtech ~ dbinom(1, p),
    logit(p) <- a + (bpC*lessexp),
    a ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  ),data=d)





##################
# Like lecture 6 #
##################

d$lessexp <- ifelse( d$category=="LE" , 1 , 0 )
m1 <- map(
  alist(
    technique ~ dbinom( tp , p),
    logit(p) <- a + bl*lessexp,
    a ~ dnorm(0,100) ,
    bl ~ dnorm(0,100)
  ) ,
  data=d )
m2 <- map(
  alist(
    technique ~ dbinom( tp , p),
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ) ,
  data=d )

compare( m1 , m2 )

precis(m1)