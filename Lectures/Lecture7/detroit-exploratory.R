################################################################################
##
## [ PROJ ] Lecture 7: Water shutoffs, race, and health in Detroit (Part 1)
## [ FILE ] detroit-exploratory.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Oct 20, 2020 >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
##
## 177,000 Detroit homes have had their water shut off for non-payment since 2010
##
## Today: who is most affected by this problem?
##
## Next week: what are the associated public health impacts?


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

#install.packages("readstata13")
#install.packages("lubridate")
#install.packages("weights")


#library(foreign)     #note only imports .dta files up thru Stata 12
library(readstata13)  #imports .dta files from Stata 13 thru 15
library(tidyverse)
library(lubridate)
library(weights)


## -----------------------------------------------------------------------------
## directory paths: make sure all input data is saved in "../Data"
## -----------------------------------------------------------------------------

getwd()


## -----------------------------------------------------------------------------
## Get demographic data
## -----------------------------------------------------------------------------

#get census tract level demographic data from ACS - confirm unit of observation
  input_acs_tract <- read.dta13("data/ACS_10_17_5YR_CensusTract.dta")

#clean, rename/construct key variables, prep for merge on a tractid
  acs_tract.clean <- input_acs_tract %>% 
    select(census_tract_long, year, num_pop_total, num_income_median, 
           per_race_black_alone_or_combo, geodisplaylabel) %>% 
    rename(tractid = census_tract_long, 
           pop = num_pop_total,
           medianinc = num_income_median,
           blackshare = per_race_black_alone_or_combo) %>% 
    mutate(black75 = as.numeric(blackshare >= 75),
           inc_above_median = as.numeric(medianinc > 26884.59)) %>%
    arrange(tractid, year)
  
  #inspect
    summary(acs_tract.clean)
    acs_tract.clean %>% filter(is.na(year) == TRUE)
    acs_tract.clean %>% filter(is.na(medianinc) == TRUE)
    table(acs_tract.clean$tractid, acs_tract.clean$year) %>% head(n = 10)


    
## -----------------------------------------------------------------------------
## Get service interruption (SI) data - shutoff records (microdata)
## -----------------------------------------------------------------------------
  
#get service interruption data
input_si <- read.dta13("data/si_1017_cleaned.dta")
  
#focus on key variables to identify period/location of every shutoff
#we'll want to join to demographic data based on tractid and get tract-level obs
si.clean <- input_si %>% 
  select(si_order_number, census_tract_long, year, month) %>% 
  rename(tractid = census_tract_long) %>% 
  arrange(tractid, year, month)

#aggregate to tract-year/month totals
si_tract_ym <- si.clean %>% 
  group_by(tractid, year, month) %>% 
  summarise(si_count = n_distinct(si_order_number)) %>% 
  arrange(tractid, year, month)

  #inspect
    summary(si_tract_ym)
    table(si_tract_ym$month, si_tract_ym$year)



## -----------------------------------------------------------------------------
## Join SI & demographic data: construct tract-month panel & tract-level totals
## -----------------------------------------------------------------------------

#join tract-year demographic data to tract-month level SI data
#want a tract-month panel
tract_ym <- left_join(si_tract_ym, acs_tract.clean, by = c("tractid", "year")) %>% 
  mutate(date = make_date(year, month, 1)) %>% 
  arrange(tractid, year, month) %>% 
  filter(date != "2017-11-01")

  #inspect
    summary(tract_ym)
    table(tract_ym$month, tract_ym$year)
    

#collapse to tract-level totals (disregard within-tract variation)
tract <- tract_ym %>% 
  group_by(tractid) %>% 
  summarise(si_count = sum(si_count),
            pop = mean(pop, na.rm = TRUE),
            blackshare = mean(blackshare, na.rm = TRUE),
            black75 = round(mean(black75, na.rm = TRUE), 0),
            medianinc = mean(medianinc, na.rm = TRUE),
            inc_above_median = round(mean(inc_above_median, na.rm = TRUE), 0) ) %>% 
  mutate(si_1000 = si_count / (pop / 1000) ) %>% #shutoffs per 1000 people
  arrange(tractid)

  #inspect
    summary(tract)


    
## -----------------------------------------------------------------------------
## Analyze relationships between tract-level income, race and SI per capita
## -----------------------------------------------------------------------------

#scatterplot: % black vs shutoffs
  ggplot(data = tract, aes(x = blackshare, y = si_1000, weight = pop, size = pop)) + 
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'lm', formula = y ~ x)  +
    scale_size(range = c(0.1, 6), guide = "none") 

  cor(tract$blackshare, tract$si_1000, use = "pairwise.complete.obs")
  wtd.cor(tract$blackshare, tract$si_1000, weight = tract$pop)
  
  
#scatterplot: median income vs shutoffs
  ggplot(data = tract, aes(x = medianinc, y = si_1000, weight = pop, size = pop)) + 
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'lm', formula = y ~ x)  +
    scale_size(range = c(0.1, 6), guide = "none") 
  
  cor(tract$medianinc, tract$si_1000, use = "pairwise.complete.obs")
  wtd.cor(tract$medianinc, tract$si_1000, weight = tract$pop)
  
  
#scatterplot: median income & race vs shutoffs
  ggplot(data = tract, 
         aes(x = blackshare, y = medianinc, size = si_1000, color = si_1000)) + 
    geom_point(alpha = 0.1) +
    scale_size(range = c(0.1, 6), guide = "none") + 
    scale_color_gradient(low="purple", high="red")

  
### WEEK 7 STOPS HERE, CONTINUE FOR WEEK 9
  
#get total population of Detroit (for simplicity, we assume the population does not change
#over time; we estimate it by summing the `pop` column in the dataframe `tract`)
  detroit_pop <- sum(tract$pop)

#time series of SI per capita
  #first, transform (aggregate the data)
  ym <- tract_ym %>% 
    group_by(date) %>% 
    summarise(si_count = sum(si_count)) %>% 
    mutate(si_1000 = si_count / (detroit_pop / 1000) )
  
  #use the transformed (aggregated) data 
  ggplot(ym, aes(x = date, y = si_1000)) + geom_line() 
  
  #get raw shutoff counts (NOT PER CAP) without transforming data first
  ggplot(tract_ym, aes(x = date, y = si_count)) +
    stat_summary(fun = sum, geom = "line")   

#get total population of Detroit tracts which are above/below median income
  detroit_pop_hi_inc <- tract %>%
    filter(inc_above_median == 1) %>%
    summarise(sum(pop)) %>%
    as.numeric()
  
  detroit_pop_lo_inc <- tract %>%
    filter(inc_above_median == 0) %>%
    summarise(sum(pop)) %>%
    as.numeric()

#time series of SI per capita for tracts above/below citywide median income
  ym_inc <- tract_ym %>% 
    group_by(date, inc_above_median) %>% 
    summarise(si_count = sum(si_count)) %>%
    mutate(pop = if_else(inc_above_median == 1, detroit_pop_hi_inc, detroit_pop_lo_inc),
           si_1000 = si_count / (pop / 1000)) %>%
    na.omit()
  
#note that we are missing one row (we only have 187 instead of 2 * 94 = 188 rows)
#the reason behind this is in Feb 2016, there is only one tract with shutoffs (whose income is below median)
#that means we are missing a row corresponding to (Feb 2016, high income) 
#in order to solve this, here is a possible solution:
  
  ym_inc <- tract_ym %>% 
    group_by(date, inc_above_median) %>% 
    summarise(si_count = sum(si_count)) %>%
    ungroup() %>%
    complete(date, inc_above_median, fill = list(si_count = 0)) %>%
    mutate(pop = if_else(inc_above_median == 1, detroit_pop_hi_inc, detroit_pop_lo_inc),
           si_1000 = si_count / (pop / 1000)) %>%
    na.omit()
  

  
  ym_inc$inc_above_median <- factor(ym_inc$inc_above_median,
                                    levels = c(0,1),
                                    labels = c("Below median income", "Above median income"))
  
  ggplot(ym_inc, aes(x = date, y = si_1000)) + 
    geom_line(aes(color = inc_above_median))

#get total population of Detroit tracts which are above/below 75% black
  detroit_pop_black <- tract %>%
    filter(black75 == 1) %>%
    summarise(sum(pop)) %>%
    as.numeric()
  
  detroit_pop_nblack <- tract %>%
    filter(black75 == 0) %>%
    summarise(sum(pop)) %>%
    as.numeric()
  
#time series of SI per capita by tracts above/below 75% black
  ym_race <- tract_ym %>% 
    group_by(date, black75) %>% 
    summarise(si_count = sum(si_count)) %>%
    ungroup() %>%
    complete(date, black75, fill = list(si_count = 0)) %>%
    mutate(pop = if_else(black75 == 1, detroit_pop_black, detroit_pop_nblack),
           si_1000 = si_count / (pop / 1000)) %>%
    na.omit()
  
  ym_race$black75 <- factor(ym_race$black75,
                            levels = c(0,1),
                            labels = c("Less than 75% Black", "At least 75% Black"))
  
  ggplot(data = ym_race, 
         aes(x = date, y = si_1000, group = black75)) + 
    geom_line(aes(color = black75))

  #note difference in raw shutoff counts (not per capita)
  ggplot(ym_race, aes(x = date, y = si_count)) + 
    geom_line(aes(color = as.factor(black75)))
  
