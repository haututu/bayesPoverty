### Pulls together poverty data online and population data I pulled from NZ.Stat to determine proportion of survey in each age group.

library(dplyr)
library(tidyr)
library(tabulizer)

### Load the poverty data ###

# URL for household income report form MSD
url <- "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/monitoring/household-income-report/2017/2017-incomes-report-wed-19-july-2017.pdf"

# Pull the AHC60 data by age
ahc60 <- data.frame(extract_tables(url, pages = 134)[[1]], stringsAsFactors = FALSE)

# Fix up column names
colnames(ahc60) <- c("year", ahc60[1,][2:6], "total")

# Convert to numeric and strip unwanted rows
ahc60 <- ahc60[2:length(ahc60$year),] %>%
  mutate_all(funs(as.numeric))

# Merge to sequence of years spanning min:max to insert missing years
ahc60 <- full_join(ahc60, data.frame(year=1986:2016), by="year") %>% 
  gather(age, pov, 2:7) %>%
  arrange(year)

### Bring in population data ###

#This involves a butt load of cleaning and turning the population counts into proportions
pop <- read.csv("data/populationEstimates.csv") %>%
  gather("year", "count", 2:14) %>%
  filter(age != "total") %>%
  mutate(year = factor(substr(year, 2, 5)),
         povAgeGrps = rep(
           c(rep("0-17 yrs", 4), rep("18-24 yrs", 1), rep("25-44 yrs", 4), rep("45-64 yrs", 4), "65+ yrs"), 
           length(levels(pop$year))
         )
  ) %>%
  group_by(year, povAgeGrps) %>%
  summarise(count = sum(count)) %>%
  group_by(year) %>%
  mutate(prop = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  spread(povAgeGrps, prop) %>%
  # Amount to take off is 2/17, a random pop pyramid from 2014 on SNZ website suggests its reasonable
  # Correct for the fact one of the age groups is off, 0-17 includes 18 and 19 yo
  mutate(`18-24 yrs` = `18-24 yrs` + 2*(`0-17 yrs`/17), 
         `0-17 yrs` = `0-17 yrs` - 2*(`0-17 yrs`/17)) %>% 
  # Approximate number of respondents multipled by avg hshd size (2.7)
  mutate(total = 1,
         survPop = as.numeric(c(rep(3000, 11), 5000, 3000) * 2.7)) %>% 
  mutate(`0-17 yrs` = `0-17 yrs` * survPop,
         `18-24 yrs` = `18-24 yrs` * survPop,
         `25-44 yrs` = `25-44 yrs` * survPop,
         `45-64 yrs` = `45-64 yrs` * survPop,
         `65+ yrs` = `65+ yrs` * survPop,
         total = total * survPop) %>%
  select(-survPop) %>%
  gather("age", "survPop", 2:7) %>%
  arrange(year)

# Merge survey estimates, convert to proportions, impute missing counts
dat <- left_join(mutate(ahc60, year = as.factor(year)), pop, by = c("year", "age")) %>%
  mutate(pov = pov / 100) %>%
  group_by(age) %>%
  mutate(survPop = imputeTS::na.interpolation(survPop)) %>%
  ungroup() %>%
  mutate(y = pov * survPop,
         exposure = survPop) %>%
  select(-survPop) %>%
  mutate(y = as.integer(y),
         exposure = as.integer(exposure))

# Save out the data
saveRDS(dat, "data/povData.RDS")



  
