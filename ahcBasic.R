library(dplyr)
library(demest)

# Load data
dat <- readRDS("data/povData.RDS") %>%
  filter(year >= 2008)

y <- xtabs(y ~ year, data=filter(dat, age == "total"), addNA = TRUE) %>%
  Counts(dimtypes = c(year = "time"), dimscales = c(year = "Points"))

exposure <- xtabs(exposure ~ year, data=filter(dat, age == "total"), addNA = TRUE) %>%
  Counts(dimtypes = c(year = "time"), dimscales = c(year = "Points"))

#Prepare covariates
covs <- read.csv("data/covariates.csv") %>%
  filter(year >= 2008)

covs <- left_join(mutate(dat, year=as.integer(year)), read.csv("data/covariates.csv"), by="year") %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

covariates.reg <- Covariates(mean ~ rent, data=covs)

prior.year <- DLM(covariates = covariates.reg)

#Set up basic model
model <- Model(y ~ Binomial(mean ~ year),
               year ~ prior.year,
               jump = 0.02)

#Estimate
filename = "out/basic.est"

estimateModel(model = model,
              y = y,
              exposure = exposure,
              filename = filename,
              nBurnin = 1000,
              nSim = 4000,
              nChain = 4,
              nThin = 5
              )

fetchSummary(filename = filename)

basic.probs <- fetch(filename = filename,
               where = c("model", "likelihood", "prob"))

fetch(filename = filename,
      where = c("model", "likelihood", "prob")) %>%
  collapseIterations(probs = c(0.025, 0.5, 0.975))

dplot(~ year,
      data = basic.probs)
