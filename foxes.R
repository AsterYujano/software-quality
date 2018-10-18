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