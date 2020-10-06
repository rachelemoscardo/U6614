################################################################################
##
## [ PROJ ] Lecture 5: Subway Fare Evasion Arrests and Racial Bias
## [ FILE ] Lecture5-startclass.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < October 6, 2020 >
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
    mutate(arrperswipe = arrests_all / (swipes2016 / 100000),
           highpov = as.numeric(povrt_all_2016 > median(st_joined_grouped$povrt_all_2016) ),
           nblack = as.numeric(shareblack > .5) )
  
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
  summary(ols1l)$coefficients[2,4] #beta1_hat
  #p-value on beta1_hat

      
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
    
    
  #calculate difference in means (weighted)
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

## -----------------------------------------------------------------------------
## 4. How does neighborhood racial composition mediate the relationship between poverty and arrest intensity
##    - examine relationship between arrest intensity & poverty by Black vs non-Black station area (nblack)
##
##    a. difference in means table: arrests per swipe by highpov vs nblack
##        - weighted by station swipes
##        - could difference in arrest intensity be explained by differences in povrt?
##
##    b. scatterplot of arrperswipe vs povrt_all by nblack
##       - add linear fit
##       - add quadratic regression fit
##
##    c. which model do you prefer, linear or quadratic?
##      - why? be clear about your logic and if applicable cite statistical evidence to support your decision
##
##    d. interpret your preferred regression specification (carefully!)
## -----------------------------------------------------------------------------
  
#4a. HINT: use tapply()
  #unweighted difference in mean arrests
  t1_arrper <- with(stations, 
                    tapply(arrperswipe, 
                           list("High Poverty" = highpov, "Predominantly Black" = nblack), 
                           mean) )
  
  #weighted difference in mean arrests
  #formula for the weighted mean = Σxw / Σw
  t1_arrper_wtd <-
    tapply(stations$arrperswipe * stations$swipes2016,
           list(stations$highpov, stations$nblack), 
           sum) / 
    tapply(stations$swipes2016,
           list(stations$highpov, stations$nblack), 
           sum)  
  
  #ok so arrest intensity is higher in high-pov stations that are majority black
  #are there differences in poverty rates that could in part explain this association?
  t1_povrt <- with(stations, 
                   tapply(povrt_all_2016, 
                          list("High Poverty" = highpov, "Predominantly Black" = nblack), 
                          mean) )    
  t1_arrper
  t1_arrper_wtd
  t1_povrt
  
  
#4b.
  #scatterplot by nblack
  ggplot(stations, aes(x = povrt_all_2016, y = arrperswipe, color = nblack)) +
    geom_point() +
    #Modify legend title and text
    scale_color_discrete(name = "Predominantly Black Station",
                         labels=c("No", "Yes"),
                         #Reverse Label Order
                         guide = guide_legend(reverse=TRUE)) +
    #Modify legend aesthetics (optional)
    theme(legend.position = "bottom", 
          legend.background = element_rect(color = "black", fill = "grey90", size = .2, linetype = "solid"), 
          legend.direction = "horizontal",
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 8) )
  
  #add linear plot

  
  #add quadratic plot


#4c.
  
  
#4d.

    
## -----------------------------------------------------------------------------
## 5. Similar to analysis above, examine relationship between arrest intensity & criminal complaints
##  
##  a. read in "nypd_criminalcomplaints_2016.csv"
##      - this csv file shows # of criminal complaints for each subway station area
##      - source: https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
##
##  b. join stations data frame to subway station area crime data
##      - join on st_id
##      - exclude the stations with the 4 highest counts of criminal complaints
##        - why? bc they are in very close proximity to Brooklyn transit policing HQ,
##          and the Brooklyn criminal courts, so they don't face comparable
##          neighborhood policing conditions
##
##  c. examine relationship between arrest intensity & criminal complaints 
##      i. First, look at over all relationship (don't take nblack into account)
##      ii. Then, allow relationship to vary by nblack
##
##  NOTE: you don't need to follow *all* the same steps in questions 2-4.
##    for 5.b.i. and 5.b.ii:
##     - focus on showing your preferred plots to inform the relationship, 
##       along w/any additional data manipulation & evidence to support your decisions/interpretation/conclusions
##     - you'll want to explore the data before arriving at your preferred plots, 
##       don't show us everything you tried, just the analysis you settled on as most relevant to this question
## -----------------------------------------------------------------------------




## -----------------------------------------------------------------------------
## 6. interpret your findings with respect to enforcement bias based on race
##    - any additional analysis you'd like to explore with the data at hand?
##    - are there any key limitations to the data/analysis affecting your
##       ability to assess enforcement bias based on race?
##    - any additional data you'd like to see that would strengthen your analysis/interpretation 
## -----------------------------------------------------------------------------  
  
  
  