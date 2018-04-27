library(dplyr)
library(demest)

# Load data
dat <- readRDS("data/povData.RDS") %>%
  filter(year >= 2008)

y <- xtabs(y ~ year + age, data=filter(dat, age != "total"), addNA = TRUE) %>%
  Counts(dimtypes = c(year = "time", age = "state"), dimscales = c(year = "Intervals"))

exposure <- xtabs(exposure ~ year + age, data=filter(dat, age != "total"), addNA = TRUE) %>%
  Counts(dimtypes = c(year = "time", age = "state"), dimscales = c(year = "Intervals"))

#Set up basic model
model <- Model(y ~ Binomial(mean ~ year + age),
               jump = 0.05)

#Estimate
filename = "out/ahcByAge.est"

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

age.probs <- fetch(filename = filename,
               where = c("model", "likelihood", "prob")) %>%
  collapseDimension(dimension = "age", weights = exposure) %>%
  collapseIterations(probs = 0.5)

dplot(~ year,
      data = age.probs)

# Looking at child poverty specifically
age.child.probs <- fetch(filename = filename,
                         where = c("model", "likelihood", "prob")) %>%
  subarray(age == "0-17 yrs") %>%
  collapseIterations(probs = c(0.025, 0.5, 0.975))

dplot(~ year,
      data = age.child.probs)
