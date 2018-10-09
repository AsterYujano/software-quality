#########
## NEW ##
#########
library(rethinking)
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
head(d)

##################
# Like lecture 6 #
##################

d$lessexp <- ifelse( d$category=="LE" , 1 , 0 )
m1 <- map(
  alist(
    technique ~ dbinom( tp , p) ,
    logit(p) <- a + bl*lessexp ,
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