library(tidyverse)
library(ggplot2) # superfluous because ggplot2 is part of tidyverse

library(haven)
library(labelled)

load(url("https://github.com/Rucla-ed/rclass2/raw/master/_data/els/els.RData"))

els_keepvars <- c(
  "STU_ID",        # student id
  "STRAT_ID",      # stratum id
  "PSU",           # primary sampling unit
  "BYRACE",        # (base year) race/ethnicity 
  "BYINCOME",      # (base year) parental income
  "BYPARED",       # (base year) parental education
  "BYNELS2M",      # (base year) math score
  "BYNELS2R",      # (base year) reading score
  "F3ATTAINMENT",  # (3rd follow up) attainment
  "F2PS1SEC",      # (2nd follow up) first institution attended
  "F3ERN2011",     # (3rd follow up) earnings from employment in 2011
  "F1SEX",         # (1st follow up) sex composite
  "F2EVRATT",      # (2nd follow up, composite) ever attended college
  "F2PS1LVL",      # (2nd follow up, composite) first attended postsecondary institution, level 
  "F2PS1CTR",      # (2nd follow up, composite) first attended postsecondary institution, control
  "F2PS1SLC"       # (2nd follow up, composite) first attended postsecondary institution, selectivity
)
els_keepvars

els <- els %>%
  # keep only subset of vars
  select(one_of(els_keepvars)) %>%
  # lower variable names
  rename_all(tolower)

els_v2 <- els %>% 
  mutate(
    hs_math = if_else(bynels2m < 0, NA_real_, as.numeric(bynels2m)),
    earn2011 = if_else(f3ern2011 < 0, NA_real_, as.numeric(f3ern2011)),
  )

els_parphd <- els_v2 %>% filter(bypared == 8)

ggplot(data= els_parphd, aes(x = hs_math, y = earn2011)) + geom_point()



#######
#ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) + 
#  geom_point()
