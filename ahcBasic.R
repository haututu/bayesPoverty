library(dplyr)
library(demest)

# Load data
dat <- readRDS("data/povData.RDS") %>%
  filter(year >= 2008)

y <- xtabs(y ~ year, data=filter(dat, age == "total"), addNA = TRUE) %>%
  Counts(dimtypes = c(year = "time"), dimscales = c(year = "Intervals"))

exposure <- xtabs(exposure ~ year, data=filter(dat, age == "total"), addNA = TRUE) %>%
  Counts(dimtypes = c(year = "time"), dimscales = c(year = "Intervals"))

#Set up basic model
model <- Model(y ~ Binomial(mean ~ year),
               year ~ prior.year,
               jump = 0.05)

#Estimate
filename = "out/basic.est"

estimateModel(model = model,
              y = y,
              exposure = exposure,
              filename = filename,
              nBurnin = 1000,
              nSim = 4000,
              nChain = 4,
              nThin = 10
              )

fetchSummary(filename = filename)

basic.probs <- fetch(filename = filename,
               where = c("model", "likelihood", "prob")) %>%
  collapseIterations(probs = 0.5)

dplot(~ year,
      data = basic.probs)
