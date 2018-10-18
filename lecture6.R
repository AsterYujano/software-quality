library(rethinking)
data(UCBadmit)
d <- UCBadmit
dl6 <- UCBadmit

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

compare( m10.6 , m10.7 )

precis(m10.6)

# m1 has the lowest WAIC, so it is the best. If there are close look at weight.