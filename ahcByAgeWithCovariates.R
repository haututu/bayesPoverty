library(dplyr)
library(demest)

# Load data
dat <- readRDS("data/povData.RDS") %>%
  filter(year >= 2008)

y <- xtabs(y ~ year + age, data=filter(dat, age != "total")) %>%
  Counts(dimtypes = c(year = "time", age = "state"), dimscales = c(year = "Intervals"))

exposure <- xtabs(exposure ~ year + age, data=filter(dat, age != "total")) %>%
  Counts(dimtypes = c(year = "time", age = "state"), dimscales = c(year = "Intervals"))

#Prepare covariates
covs <- read.csv("data/covariates.csv") %>%
  filter(year >= 2008)

covariates.reg <- Covariates(mean ~ rent * income + highDep, data=covs)

prior.year <- Exch(covariates = covariates.reg)

#Set up basic model
model <- Model(y ~ Binomial(mean ~ year * age),
               year ~ prior.year,
               jump = 0.05)

#Estimate
filename = "out/ahcByAgeWithCovariates.est"

estimateModel(model = model,
              y = y,
              exposure = exposure,
              filename = filename,
              nBurnin = 1000,
              nSim = 10000,
              nChain = 4,
              nThin = 20
)

fetchSummary(filename = filename)

age.probs <- fetch(filename = filename,
               where = c("model", "likelihood", "prob")) %>%
  collapseDimension(dimension = "age", weights = exposure) #%>%
  #collapseIterations(probs = c(0.5))

totalDirect <- filter(dat, age == 'total') %>%
  xtabs(pov ~ year, data=.) %>%
  Values(dimscales = c(year = "Points"))

dplot(~ year,
      data = age.probs,
      overlay = list(values = totalDirect, col = "black", lwd = 8))

# Looking at child poverty specifically
age.child.probs <- fetch(filename = filename,
                         where = c("model", "likelihood", "prob")) %>%
  subarray(age == "0-17 yrs") %>%
  collapseIterations(probs = c(0.025, 0.5, 0.975))

dplot(~ year,
      data = age.child.probs,
      overlay = list(values = filter(dat, age == 'total')$pov, col = "black", lwd = 2))

