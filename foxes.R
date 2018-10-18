library(rethinking)
data(foxes)
d <- foxes
m1 <- map(
  alist(
    weight ~ dnorm( mu, sigma),
    mu <- a + ba*area,
    a ~ dnorm(0,100),
    ba ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
m2 <- map(
  alist(
    weight ~ dnorm( mu, sigma),
    mu <- a + bg
    *
      groupsize,
    a ~ dnorm(0,100),
    bg ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(m1)
precis(m2)


#########

library(rethinking)
data(chimpanzees)
d <- chimpanzees
d2 <- d
d2$recipient <- NULL
m10.4_map <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  ),data=d2)
  
  
  