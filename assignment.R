dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
head(d)

m1 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba*area,
    a ~ dnorm(0, 100),
    ba ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), 
  data =d
)
