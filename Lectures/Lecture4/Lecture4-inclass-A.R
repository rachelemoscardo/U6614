################################################################################
##
## [ PROJ ] Lecture 4: Subway Fare Evasion Arrests and Racial Bias
## [ FILE ] Lecture4-inclass.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Sept 29, 2020 >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
## Police can stop and ticket or arrest people for subway fare evasion. 
## Is NYPD enforcement of subway fare evasion enforcement in Brooklyn racist?


## ---------------------------
## libraries
## ---------------------------

library(tidyverse)
library(weights)
library(lmtest)
library(sandwich)

## ---------------------------
## directory paths
## ---------------------------

getwd()

## -----------------------------------------------------------------------------
## 1. Subway station-level arrest totals
##
##  a. load cleaned/appended arrest microdata (arrests_all.csv) w/all strings as factors
##
##  b. aggregate microdata to station-level observations w/the following info:
##      - st_id, loc2, arrests_all (=arrest count)
##      - store results as new data frame st_arrests
##
##  c. plot histogram of arrests and briefly describe distribution of arrests across stations
## -----------------------------------------------------------------------------

#1a.
arrests_all <- read.csv("arrests_all.csv", 
                        stringsAsFactors = TRUE,
                        na.strings = c("")) #can specify string values to read in as NA (here blanks)
#1b.
  st_arrests <- arrests_all %>% 
    group_by(st_id, loc2) %>% 
    summarise(arrests_all = n() ) %>% 
    arrange(desc(arrests_all))

  #could use count() in place of group_by() & summarise() - shorter but less flexible
  arrests_all %>% 
    count(st_id, loc2) %>% 
    arrange(desc(n))
  
#1c.
  ggplot(data = st_arrests, aes(x = arrests_all)) + geom_histogram()


  ## -----------------------------------------------------------------------------
  ## 2. joining ridership and neighborhood demographics to arrest data
  ##
  ##  a. import other station-level datasets and inspect
  ##  
  ##  b. join both files to st_arrests and inspect results (store new df as st_joined),
  ##      - inspect results of join and describe any issues
  ##      - drop unnecessary columns from the ridership data
  ##      - group st_joined by st_id and mta_name
  ##
  ##  c. print top 10 stations by arrest counts w/povrt_all and shareblack cols
  ## -----------------------------------------------------------------------------

  #2a. 
  st_poverty <- read.csv("station_povdataclean_2016.csv", 
                         stringsAsFactors = TRUE)
  
  #poverty data (from 2016 American Community Survey): 
  #  Source: https://usa.ipums.org/usa/acs.shtml
  #  each unit represents a "subway station area", defined as follows:
  #    all census tracts w/geometric center within .5km of a station (see map)
  #  each observation is a subway station area with a unique identifier (st_id)
  #  povrt_all_2016: % of adults in subway station area living below federal poverty level
  #  shareblack: share of adults in subway station area who identify as Black
  #  nblack: dummy variable = 1 if shareblack >= 50%, 0 otherwise
  
  
  st_ridership <- read.csv("Subway Ridership by Station - BK.csv", 
                           stringsAsFactors = TRUE)
  #MTA ridership data:
  #  Source: http://web.mta.info/nyct/facts/ridership/ridership_sub_annual.htm
  #  each observation is a subway station (area) w/a unique identifier (st_id)
  #  includes annual # of MetroCard swipes at each station for 2011-16

str(st_poverty)
str(st_ridership)


#2b.
  #a vector of columns we don't need to keep
  drop_vars <- c("swipes2011", "swipes2012", "swipes2013", "swipes2014", "swipes2015")
  
  st_joined_grouped <- inner_join(st_poverty, st_ridership) %>%
    inner_join(st_arrests) %>% 
    select(!drop_vars) %>% 
    group_by(st_id, mta_name) 
  
  #inspect
  str(st_joined_grouped)
  summary(st_joined_grouped)
    #Note: 157 obs in joined df w/no NAs (except missing demographics) 
    #inner join worked as intended!

#2c.
  st_joined_grouped %>% 
    arrange(desc(arrests_all)) %>% 
    select(st_id, mta_name, arrests_all, shareblack, povrt_all_2016) %>% 
    head(n = 10) 


## -----------------------------------------------------------------------------
## 3. Explore relationship between arrest intensity & poverty rates across stations
##
##  a. compute arrest intensity variable and prep data
##      - exclude coney island station from the analysis sample
##        - why? a popular destination for young people from other neighborhoods
##        - arguably diff relationship between neighborhood characteristics & arrests
##      - create new variable measuring enforcement intensity:
##        - arrperswipe_2016 = arrests per 100,000 swipes
##      - create new dummy variable indicating high poverty station area:
##        - highpov = 1 if pov rate is > median pov rate across stations
##      - create new dummy for majority Black station areas (shareblack > .5)
##      - coerce new dummies into factors w/category labels
##     - assign results to new df called stations
##     - validate results as needed!
##
##  b. investigate arrests intensity vs poverty rates
##     - plot arrperswipe vs povrt_all_2016
##     - investigate linear and quadratic model fit 
##     - report diff in mean arrest intensity between high/low pov areas
##        - is diff significant?
##        - weight observations by swipes for difference in group means
## ---------------------------------------------------------------------------

#3a.
  stations <- st_joined_grouped %>% 
    filter(st_id != 66) %>% 
    mutate(arrperswipe = arrests_all / (swipes2016 / 100000), #station swipe intensity
           highpov = as.numeric(povrt_all_2016 >= median(st_joined_grouped$povrt_all_2016)),
           nblack = as.numeric(shareblack > .5) )
  #note we can directly test condition as a logical comparison
  #here we convert logical results into a numeric (i.e. a dummy var!)
  
  
  #crosstab highpov and black
  table(stations$highpov, stations$nblack)

  #let's encode highpov as a factor with nice labels
  stations$highpov <- factor(stations$highpov,
                             levels = c(0,1),
                             labels = c("Not high poverty", "High poverty"))
  
  #let's encode highpov as a factor with nice labels
  stations$nblack <- factor(stations$nblack,
                            levels = c(0,1),
                            labels = c("Majority non-Black", "Majority Black"))
  
  table(stations$highpov, stations$nblack)
  

  #validation: now check top 10 stations by arrest intensity
  stations %>% 
    arrange(desc(arrperswipe)) %>% 
    select(st_id, mta_name, arrperswipe, arrests_all, shareblack, povrt_all_2016, highpov, nblack) %>% 
    head(n = 10)
  
#3b. 
  ggplot(stations, #specify dataframe to use
         aes(x = povrt_all_2016, y = arrperswipe)) + #specify columns to use
    geom_point() + #specify plot geometry
    ggtitle('Scatterplot of arrest rate vs. poverty rate') + #add title
    labs(x = 'poverty rate', y = 'arrest rate') #change axis labels

  #fit linear OLS model (arrest rate vs. poverty rate)
  ols1l <- lm(arrperswipe ~ povrt_all_2016, data = stations)
  
  #linear model with station observations weighted by swipes
  ols1l <- lm(arrperswipe ~ povrt_all_2016, data = stations, weights = swipes2016)
  summary(ols1l) #get summary of the model
  coeftest(ols1l, vcov = vcovHC(ols1l, type="HC1")) #get robust SEs
  
  #how to refer to specific regression results
  ?summary.lm
  summary(ols1l)$adj.r.squared  #adj R-square
  summary(ols1l)$coefficients   #coefficients
  round(summary(ols1l)$coefficients[2,1],2) #beta1_hat
  coeftest(ols1l, vcov = vcovHC(ols1l, type="HC1"))[2,4] #p-value on beta1_hat
  

  #add linear prediction line to scatter plot
  ggplot(stations, 
         aes(x = povrt_all_2016, y = arrperswipe)) + 
    geom_point() + 
    ggtitle('Scatterplot of arrest rate vs. poverty rate') + 
    labs(x = 'poverty rate', y = 'arrest rate') + 
    geom_smooth(method = 'lm', formula = y ~ x) #add regression line
  
  #plot linear prediction line w/weighted observations
  ggplot(stations, 
         aes(x = povrt_all_2016, y = arrperswipe, weight = swipes2016)) + 
    geom_point() + 
    ggtitle('Scatterplot of arrest rate vs. poverty rate') + 
    labs(x = 'poverty rate', y = 'arrest rate') + 
    geom_smooth(method = 'lm', formula = y ~ x) 

  
  #fit quadratic OLS model (arrest rate vs. poverty rate)
  #HINT: see quadratic syntax from Lecture4.2 (section 4.1)
  

  #add quadratic prediction line to scatter plot

  
  
  #calculate difference in means between high/low poverty stations (unweighted)
  stations %>% 
    ungroup() %>% 
    group_by(highpov) %>% 
    summarise(n = n(),
              mean_pov = mean(povrt_all_2016),
              mean_arrper = mean(arrperswipe))

    #inference with t.test command and unequal variance
    t.test(arrperswipe ~ highpov, data = stations, var.equal = FALSE)

    #inference with bivariate regression and robust SE coefficient test
    diff1 <- lm(arrperswipe ~ highpov, data = stations)
    summary(diff1) #get summary of the model
    coeftest(diff1, vcov = vcovHC(diff1, type="HC1")) #get robust SEs
    
    
  #difference in means (weighted)
  stations %>% 
    ungroup() %>% 
    group_by(highpov) %>% 
    summarise(n = n(),
              mean_pov = weighted.mean(povrt_all_2016, swipes2016),
              mean_arrper = weighted.mean(arrperswipe, swipes2016))
    #?weighted.mean
  
  stations_highpov <- stations %>% filter(highpov == "High poverty")
  stations_lowpov  <- stations %>% filter(highpov == "Not high poverty")
  wtd.t.test(stations_highpov$arrperswipe, stations_lowpov$arrperswipe, 
             weight = stations_highpov$swipes2016, 
             weighty = stations_lowpov$swipes2016)
    #?wtd.t.test
  
  