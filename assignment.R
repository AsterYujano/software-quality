dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")
head(d)

mstandart <- map(
  alist(
    tp ~ dnorm(mu, sigma),
    mu <- a + bc*category + bt*technique,
    a ~ dnorm(0, 100),
    c(bc, bt) ~ dnorm(0, 20),
    sigma ~ dunif(0,50)
  ),
  data = d
)
