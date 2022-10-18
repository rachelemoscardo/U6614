################################################################################
##
## [ PROJ ] Lecture 7: Water shutoffs, race, and health in Detroit (Part 1)
## [ FILE ] detroit-exploratory.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Oct 13, 2022 >
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
## directory paths
## -----------------------------------------------------------------------------

#this time we have a parent folder for the project (DetroitWaterShutoffs),
#and two subfolders for Data and Code 
getwd()


## -----------------------------------------------------------------------------
## Get demographic data
## -----------------------------------------------------------------------------

#get census tract level demographic data from ACS 
  input_acs_tract <- read.dta13("data/ACS_10_17_5YR_CensusTract.dta")

  #unit of observation?
  
  
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
  
    #what is the unit of observation?
      table(acs_tract.clean$tractid, acs_tract.clean$year) %>% head(n = 10)
        #we have a balanced year-tract panel!
    
    #why the NA values?
      acs_tract.clean %>% filter(is.na(year) == TRUE)
      acs_tract.clean %>% filter(is.na(medianinc) == TRUE)

    
## -----------------------------------------------------------------------------
## Get service interruption (SI) data - i.e. shutoff records (microdata)
## -----------------------------------------------------------------------------
  
#get service interruption data
input_si <- read.dta13("data/si_1017_cleaned.dta")
  
  #what is the unit of observation? each observation is a shutoff record.
      
    
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
    table(si_tract_ym$month, si_tract_ym$year) #what does this tell us?


## -----------------------------------------------------------------------------
## Join shutoff & demographic data: construct tract-year/month panel w tract-level totals
## -----------------------------------------------------------------------------

#join tract-year demographic data (acs_tract.clean) to tract-month shutoff data (si_tract_ym)
#only keep tracts that are in the shutoff data (si_tract_ym)
    #acs_tract.clean includes Detroit tracts (Wayne County), 
    #but also tracts outside of Detroit across the state of Michigan
#want to end up with a tract-year-month panel
#new df should include: all columns from two dfs and a new date column
#also filter out observation for 2017-11-01
#HINT: what column(s) do you want to join on?
#HINT: what kind of join would work here?
    
tract_ym <- left_join(si_tract_ym, acs_tract.clean,
                      by = c("tractid", "year")) %>% 
  mutate(date = make_date(year, month, 1)) %>% 
  arrange(tractid, year, month) %>% 
  filter(date != "2017-11-01")

  #inspect
  summary(tract_ym)
    
  #do we have a balanced panel? 
  table(tract_ym$month, tract_ym$year)
  tract_ym %>% group_by(geodisplaylabel) %>% count(geodisplaylabel)
    #nope. if a tract had 0 shutoffs one month, there's no row for that county-month
    

#collapse to tract-level totals (summed over all year-months)
#i.e. a single obs per tract with shutoffs summed over all years
#include time-invariant measures of other variables
#NOTE: this allows us to focus on variation between tracts (not within-tracts over time)
  #will no longer be a panel dataframe
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
  
  #what is the unit of observation? 1 observation for each census tract.

    
## -------------------------------------------------------------------------------------
## 1. Cross-sectional analysis of relationships between tract-level income, race & shutoffs
## -------------------------------------------------------------------------------------

#NOTE: here "cross-sectional" means 1 obs/tract (w/shutoffs summed over all years)
    
  #scatterplot: % black vs shutoffs
    ggplot(data = tract, 
           aes(x = blackshare, y = si_1000)) + 
      geom_point() +
      geom_smooth(method = 'lm', formula = y ~ x) 
    
    
  #QUESTION: how can we improve the above plot?
    #do you want to consider weighting observations?
    #how can we use aesthetic mappings to incorporate weighting in our visualization?  
    
#scatterplot: % black vs shutoffs
  ggplot(data = tract, aes(x = blackshare, 
                           y = si_1000, 
                           weight = pop, 
                           size = pop)) + 
    geom_point(alpha = 0.1) + #alpha adjusts the transparency of points
    geom_smooth(method = 'lm', formula = y ~ x)  +
    scale_size(range = c(0.1, 6), guide = "none") 

  #compute correlation for assessing model fit (in addition to visual inspection)
    cor(tract$blackshare, tract$si_1000, use = "pairwise.complete.obs")
    wtd.cor(tract$blackshare, tract$si_1000, weight = tract$pop)
  
  
#scatterplot: median income vs shutoffs
FILL IN CODE SIMILAR TO ABOVE BUT USE medianinc RATHER THAN blackshare
  
  #correlations for model fit
   FILL IN CODE SIMILAR TO ABOVE
  
  
#scatterplot: median income & race vs shutoffs
  #HINT: try plotting Black share on the X-axis and median income on the Y-axis
  #      then experiment w/different aesthetics to indicate greater shutoff rates
  #      e.g. focus on marker size, color, transparency, etc.
  ggplot(data = tract, 
         aes(x = blackshare, 
             y = medianinc, 
             FILL IN OTHER AESTHETIC MAPPING ARGUMENTS)) + 
    geom_point(alpha = 0.1) #alpha adjusts the transparency of points 
  #HINT: try ?scale_size(), this is a function to adjust the size aesthetic
  #HINT: try ?scale_color_gradient() to see how to create a diverging color gradient

  
## -----------------------------------------------------------------------------------
## 2.0 Time series plots of citywide shutoffs (aggregate across all tracts in every month)
## -----------------------------------------------------------------------------------  

#get total population of Detroit (for simplicity, assume pop doesn't change over time
#we estimate it by summing the `pop` column in the dataframe `tract`)
  detroit_pop <- sum(tract$pop)
  
  
#first let's get citywide time series of shutoffs per capita
#aggregate tract-level observations in tract_ym into 1 observation for every month
  ym <- tract_ym %>% 
    group_by(date) %>% 
    summarise(si_count = sum(si_count)) %>% 
    mutate(si_1000 = si_count / (detroit_pop / 1000) )
  
  #use the transformed (aggregated) data 
  ggplot(ym, aes(x = date, y = si_1000)) + geom_line() 
  
  #get raw shutoff counts (NOT PER CAP) without transforming data first
  ggplot(tract_ym, aes(x = date, y = si_count)) +
    stat_summary(fun = sum, geom = "line")   

  
## -----------------------------------------------------------------------------------
## 2.1 Time series plots of shutoffs by income level of Census tracts (above/below median)
## -----------------------------------------------------------------------------------
  
#get total population of Detroit tracts which are above/below median income
  detroit_pop_hi_inc <- tract %>%
    filter(inc_above_median == 1) %>%
    summarise(sum(pop)) %>%
    as.numeric()
  
  detroit_pop_lo_inc <- tract %>%
    filter(inc_above_median == 0) %>%
    summarise(sum(pop)) %>%
    as.numeric()

  
#time series of shutoffs per capita for tracts above/below citywide median income
  #what should the unit of observation be for this new data frame?
  ym_inc <- tract_ym %>% 
    group_by(date, inc_above_median) %>% 
    summarise(si_count = sum(si_count)) %>%
    mutate(pop = if_else(inc_above_median == 1, 
                         detroit_pop_hi_inc, 
                         detroit_pop_lo_inc),
           si_1000 = si_count / (pop / 1000)) %>%
    na.omit()
  
#note that we are missing 1 row (we only have 187 instead of 2 * 94 = 188 rows)
#the reason is that in Feb 2016 there is only 1 tract with shutoffs (w/income below the median)
#that means we are missing a row corresponding to Feb 2016, high income
#in order to solve this, here is a possible solution:
  
  ym_inc <- tract_ym %>% 
    group_by(date, inc_above_median) %>% 
    summarise(si_count = sum(si_count)) %>%
    na.omit() %>% 
    ungroup() %>%
    complete(date, 
             inc_above_median, 
             fill = list(si_count = 0)) %>% #this fills in a new obs for Feb 2016
    mutate(pop = if_else(inc_above_median == 1, 
                         detroit_pop_hi_inc, 
                         detroit_pop_lo_inc),
           si_1000 = si_count / (pop / 1000)) 

#plot time series: separate lines for tracts above/below median income
  ggplot(ym_inc, 
         aes(x = date, y = si_1000)) + 
    geom_line(aes(group = inc_above_median, 
                  color = inc_above_median))
  #QUESTION: why did we specify both group and color aes() arguments?


#QUESTION:
  #should inc_above_median be treated as a numeric variable?
  #what is a better way to store this information?
  
#ANSWER: let's convert to a factor that works better with plots
  #turn inc_above_median into a factor with clear category labels
  ym_inc$inc_above_median <- factor(ym_inc$inc_above_median,
                                    levels = c(0,1),
                                    labels = c("Below median income", "Above median income"))

  
#now plot time series by income group again using a factor for the color argument
  ggplot(ym_inc, 
         aes(x = date, y = si_1000, color = inc_above_median)) + 
    geom_line()

#note that here the color aesthetic can be set in ggplot() or geom_line()
  ggplot(ym_inc, 
         aes(x = date, y = si_1000)) + 
    geom_line(aes(color = inc_above_median))
  

  
## -----------------------------------------------------------------------------
## 2.2 Time series plots of shutoffs by racial composition of Census tracts
## -----------------------------------------------------------------------------
   
#get total population of Detroit tracts which are above/below 75% black
  detroit_pop_black <- tract %>%
    filter(black75 == 1) %>%
    summarise(sum(pop)) %>%
    as.numeric()
  
  detroit_pop_nblack <- tract %>%
    filter(black75 == 0) %>%
    summarise(sum(pop)) %>%
    as.numeric()

    
#get time series of shutoffs per capita by tracts above/below 75% black
  ym_race <- tract_ym %>% 
    group_by(date, black75) %>% 
    summarise(si_count = sum(si_count)) %>%
    na.omit() %>% 
    ungroup() %>%
    complete(date, black75, fill = list(si_count = 0)) %>%
    mutate(pop = if_else(black75 == 1, 
                         detroit_pop_black, 
                         detroit_pop_nblack),
           si_1000 = si_count / (pop / 1000))

  
#convert black75 in the ym_race data frame to a factor w/clear labels for a nicer plot
  FILL IN CODE
  
#plot time series by tracts above/below 75% black 
  FILL IN CODE TO CALL ggplot() FUNCTION
  
