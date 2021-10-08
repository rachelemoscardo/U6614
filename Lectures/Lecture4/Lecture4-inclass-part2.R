################################################################################
##
## [ PROJ ] Lecture 4: Subway Fare Evasion Arrests and Racial Bias (part 1)
## [ FILE ] Lecture4-inclass.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Oct 5, 2021 >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
## Police can stop and ticket or arrest people for subway fare evasion. 
## Is NYPD enforcement of subway fare evasion enforcement in Brooklyn racist?


## --------------------------------------
## 1. load libraries and check directory
## --------------------------------------

#install.packages("weights")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("knitr")

library(tidyverse)
library(weights)
library(lmtest)
library(sandwich)
library(knitr)

getwd()

## -----------------------------------------------------------------------------
## 2. Aggregating to subway station-level arrest totals
##
##  a. load cleaned/appended arrest microdata (arrests_all.csv) w/all strings as factors
##
##  b. aggregate microdata to station-level observations w/the following info:
##      - st_id, loc2, arrests_all (=arrest count)
##      - store results as new data frame st_arrests
##
##  c. plot histogram of arrests and briefly describe distribution of arrests across stations
## -----------------------------------------------------------------------------

#2a.
  arrests_all <- read.csv("arrests_all.csv", 
                         stringsAsFactors = TRUE,
                          na.strings = c("")) #specify string values to read in as NA (here blanks)
#2b.
  st_arrests <- arrests_all %>% 
    group_by(st_id, loc2) %>% 
    summarise(arrests_all = n() ) %>% 
    arrange(desc(arrests_all))

  #could use count() in place of group_by() & summarise() - shorter but less flexible
  arrests_all %>% 
    count(st_id, loc2) %>% 
    arrange(desc(n))
  
#2c.
  ggplot(data = st_arrests, aes(x = arrests_all)) + geom_histogram()


## -----------------------------------------------------------------------------
## 3. joining ridership and neighborhood demographics to arrest data
##
##  a. import other station-level datasets and inspect
##      - don't include inspection code in your rmd submission
##  
##  b. join both files to st_arrests and inspect results (store new df as st_joined),
##      - inspect results of join and describe any issues
##      - drop unnecessary columns from the ridership data
##      - group st_joined by st_id and mta_name
##      - inspect but don't include inspection code in your rmd submission
##        - confirm you have 157 station-level observations
##
##  c. print top 10 stations by arrest counts
##      - only display st_id, mta_name, arrests_all, shareblack, povrt_all_2016
## -----------------------------------------------------------------------------

#3a. 
  st_poverty <- read.csv("station_povdataclean_2016.csv", 
                         stringsAsFactors = TRUE)
  
  #background on poverty data (from 2016 American Community Survey): 
  #  Source: https://usa.ipums.org/usa/acs.shtml
  #  each observation represents a "subway station area", defined as follows:
  #   - all census tracts w/geometric center within .5km of a station (see map)
  #  st_id is the unique identifier for each subway station area
  #  povrt_all_2016: % of adults in subway station area living below federal poverty level
  #  shareblack: share of adults in subway station area who identify as Black
  #  nblack: dummy variable = 1 if shareblack >= 50%, 0 otherwise
  
  
  st_ridership <- read.csv("Subway Ridership by Station - BK.csv", 
                           stringsAsFactors = TRUE)
  #background on MTA ridership data:
  #  Source: http://web.mta.info/nyct/facts/ridership/ridership_sub_annual.htm
  #  each observation is a subway station w/a unique identifier (st_id)
  #  includes annual # of MetroCard swipes at each station for 2011-16

  #make sure to inspect these new df's before we join them in 3b!

  st_ridership$mta_name %>% n_distinct()
  st_poverty$mta_name %>% n_distinct()
  c(st_ridership$mta_name, st_poverty$mta_name)

#3b.
  #a vector of columns we don't need to keep
    drop_vars <- c("swipes2011", "swipes2012", "swipes2013", "swipes2014", "swipes2015")
  
  #in-class exercise: join all 3 data frames (in a single pipe if you can):
    #3 data frames to join: st_poverty, st_ridership, st_arrests   
  st_joined <- inner_join(st_poverty, st_ridership) %>%
    inner_join(st_arrests) %>% 
    select(!drop_vars) %>% 
    group_by(st_id, mta_name) 
  
  #alternative method, step-by-step pipe
  st_joined <- st_poverty %>% 
    inner_join(st_ridership) %>%
    inner_join(st_arrests) %>% 
    select(!drop_vars) %>% 
    group_by(st_id, mta_name) 

  #inspect - DO NOT INCLUDE IN RMD SUBMISSION
    str(st_joined)  #why is this so long? 
    summary(st_joined)
      #Note: 157 obs in joined df w/no NAs (except some missing demographics) 
      #inner join worked as intended!

    
#3c.
  st_joined %>% 
    arrange(desc(arrests_all)) %>% 
    select(st_id, mta_name, arrests_all, shareblack, povrt_all_2016) %>% 
    head(n = 10) 


## -----------------------------------------------------------------------------
## 4. Explore relationship between arrest intensity & poverty rates across stations
##
##  a. compute variables for arrest intensity and other explanatory variables
##     - exclude coney island station from the analysis sample
##     - create new variable measuring fare evasion arrest intensity:
##       - arrperswipe_2016 = arrests per 100,000 swipes
##     - create new dummy variable indicating high poverty station area:
##       - highpov = 1 if pov rate is > median pov rate across stations
##     - create new dummy for majority Black station areas (shareblack > .5)
##     - coerce new dummies into factors w/category labels
##     - assign results to new df called stations
##     - validate results 
##     - display top 10 stations by arrest intensity using kable() in knitr package
##       - only show st_id, mta_name, arrests_all and new columns
##
##  b. investigate arrests intensity vs poverty rates 
##     - plot arrperswipe vs povrt_all_2016
##     - should we weight stations by # of MetroCard swipes?
##     - investigate linear and quadratic model fit 
##  
##  c. report diff in mean arrest intensity between high/low pov areas
##     - weight observations by swipes for difference in group means
##     - is this difference statistically significant?
## ---------------------------------------------------------------------------

#4a.
  stations <- st_joined %>%
    filter(st_id != 66) %>%
    mutate(arrperswipe = round(arrests_all / (swipes2016 / 100000), 2),
           highpov = as.numeric(povrt_all_2016 > median(st_joined$povrt_all_2016)),
           nblack = as.numeric(shareblack > .5), 
           highpov = factor(highpov, levels = c(0,1), 
                            labels = c("Not high poverty", "High poverty")),
           nblack  = factor(nblack, levels = c(0,1), 
                            labels = c("Majority non-Black", "Majority Black")),
           shareblack = round(shareblack, 2),
           povrt_all_2016 = round(povrt_all_2016, 2)) 
  
  #note we can directly test conditions as a logical comparison
  #then we convert logical results into numeric (i.e. a dummy variable)
  #we also continued on and converted to factors
  
  #conceptual exercise - the below has the exact same result as above
  stations <- st_joined %>%
    filter(st_id != 66) %>%
    mutate(arrperswipe = round(arrests_all / (swipes2016 / 100000), 2),
           highpov = povrt_all_2016 > median(st_joined$povrt_all_2016),
           nblack = shareblack > .5, 
           highpov = factor(highpov, levels = c(FALSE,TRUE), 
                            labels = c("Not high poverty", "High poverty")),
           nblack  = factor(nblack, levels = c(FALSE,TRUE), 
                            labels = c("Majority non-Black", "Majority Black")),
           shareblack = round(shareblack, 2),
           povrt_all_2016 = round(povrt_all_2016, 2))

  
  #some inspection and validation
  
    #check if nblack recoding worked as intended
      stations %>% 
        group_by(nblack) %>% 
        summarise(min(shareblack), mean(shareblack), max(shareblack))

    #examine joint distribution of highpov and black
      table(stations$highpov, stations$nblack)
      
  
  #display top 10 stations by arrest intensity (show st_id, mta_name, arrests_all and new variables)
  stations %>% 
    arrange(desc(arrperswipe)) %>% 
    select(st_id, mta_name, arrperswipe, arrests_all, shareblack, povrt_all_2016, highpov, nblack) %>% 
    head(n = 10) %>% 
    kable() #kable offers better table formatting
  

  
#4b. 
  ggplot(stations, #specify dataframe to use
         aes(x = povrt_all_2016, y = arrperswipe)) + #specify columns to use
    geom_point() + #specify plot geometry
    ggtitle('Scatterplot of arrest intensity vs. poverty rate') + #add title
    labs(x = 'poverty rate', y = 'arrests relative to ridership') #change axis labels

  #fit linear model with station observations weighted by swipes
    ols1l <- lm(arrperswipe ~ povrt_all_2016, data = stations)
    summary(ols1l) #get summary of the model
    
    #you can also store the summary object to access its elements more easily 
    ols_summary_obj <- summary(ols1l)
    ols_summary_obj$r.squared
    
    #get robust SEs #look up docs for sandwich
    coeftest(ols1l, vcov = vcovHC(ols1l, type="HC1")) 
                             
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
      ggtitle('Scatterplot of arrest intensity vs. poverty rate') + 
      labs(x = 'poverty rate', y = 'arrests relative to ridership') + 
      geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) #add linear SRF
  
  #fit quadratic OLS model (arrest intensity vs. poverty rate)
  #HINT: see quadratic syntax from Lecture4.2 (section 4.1)
    ols1q <- lm(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2),
               data = stations) #include quadratic term
    summary(ols1q) 
    coeftest(ols1q, vcov = vcovHC(ols1q, type="HC1"))

  #add quadratic prediction line to scatter plot
    ggplot(stations,
           aes(x = povrt_all_2016, y = arrperswipe)) + 
      geom_point() + 
      ggtitle('Linear regression fit') + 
      labs(x = 'poverty rate', y = 'arrests relative to ridership') + 
      geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) 

  
#4c. calculate and test difference in means between high/low poverty stations
    
  #summarise with group_by is ok... but doesn't accept weights!
    stations %>%
      group_by(highpov) %>% 
      summarise(n = n(),
                mean_pov = mean(povrt_all_2016),
                mean_arrper = mean(arrperswipe))

     #inference with t.test command and unequal variance (doesn't accept weights!)
     #default t-test setting is unequal variance, we can coerce it to true
      vareq_t_test <- t.test(arrperswipe ~ highpov, data = stations, var.equal=TRUE)
      vareq_t_test
      vareq_t_test$stderr
   
      #note this is same stderr as unweighted bivariate regression
      summary(lm(arrperswipe ~ highpov, data = stations))$coefficients[2,2]
    
  #instead let's use bivariate regression (accepts weights!) w/robust SEs
    #QUANT II REVIEW: 
    # equivalence of diff-in-means test & bivariate regression w/dummmy regressor
    # consult Video Lecture 2.2a on the class website 
    diff1 <- lm(arrperswipe ~ highpov, data = stations, weights = swipes2016)
    summary(diff1) #get summary of the model
    coeftest(diff1, vcov = vcovHC(diff1, type="HC1")) #get robust SEs
    
  #wtd.t.test function in the weights package accepts weights... but not robust SEs
    ?wtd.t.test
    stations_highpov <- stations %>% filter(highpov == "High poverty")
    stations_lowpov  <- stations %>% filter(highpov == "Not high poverty")
    wtd.t.test(stations_highpov$arrperswipe, stations_lowpov$arrperswipe, 
               weight = stations_highpov$swipes2016, 
               weighty = stations_lowpov$swipes2016)
      #compare difference and SE of difference to previous approach
    

## -----------------------------------------------------------------------------
## 5. How does neighborhood racial composition mediate the relationship between poverty and arrest intensity
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
    
#5a. HINT: use tapply()
  #unweighted difference in mean arrests
    t1_arrper <- with(stations, 
                      tapply(arrperswipe, 
                             list("High Poverty" = highpov, 
                                  "Predominantly Black" = nblack), 
                             mean))
    t1_arrper
  
  #weighted difference in mean arrests
  #formula for the weighted mean = Σxw / Σw
    t1_arrper_wtd <-
      tapply(stations$arrperswipe * stations$swipes2016,
             list(stations$highpov, 
                  stations$nblack), 
             sum) / 
      tapply(stations$swipes2016,
             list(stations$highpov, 
                  stations$nblack), 
             sum)  

  #ok so arrest intensity is higher in high-pov stations that are majority black
  #are there differences in poverty rates that could in part explain this association?
    t1_povrt <- with(stations, 
                     tapply(povrt_all_2016, 
                            list("High Poverty" = highpov, 
                                 "Predominantly Black" = nblack), 
                            mean))    
    t1_povrt
    t1_arrper
    t1_arrper_wtd
    
#5b.
  #scatter plot by nblack w/linear plots
    ggplot(stations, aes(x = povrt_all_2016, y = arrperswipe, color = nblack)) +
      geom_point()  +
      #geom_smooth(method = 'lm', formula = y ~ x) +    ###add linear fit
      #geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) ###add quadratic fit
      ylab("Arrests relative to ridership") + xlab("Station area poverty rate") +
      ggtitle("Fare Evasion Arrest Intensity vs Poverty by Race", 
        subtitle = "Subway stations in Brooklyn (2016)") +
      scale_color_discrete(name = "Predominantly Black Station",
                           labels=c("No", "Yes"),
                           guide = guide_legend(reverse=TRUE)) +
      theme(legend.position = "bottom", 
            legend.background = element_rect(color = "black", fill = "grey90", size = .2, linetype = "solid"), 
            legend.direction = "horizontal",
            legend.text = element_text(size = 8), 
            legend.title = element_text(size = 8) )
  
  #scatterplot by nblack w/quadratic plots - as above
    

#5c.  
  
    #by predominantly Black stations: get separate data frames
    stations_black <- stations %>% filter(nblack == "Majority Black")
    stations_nonblack <- stations %>% filter(nblack == "Majority non-Black")
    
    #nblack == 1: linear model with station observations weighted by swipes
    ols1l <- lm(arrperswipe ~ povrt_all_2016, data = stations_black, weights = swipes2016)
    summary(ols1l) #get summary of the model
    
    #nblack == 1: quadratic model with station observations weighted by swipes
    ols1l <- lm(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2), data = stations_black, weights = swipes2016)
    summary(ols1l) #get summary of the model

  
#5d.
  
    
## -----------------------------------------------------------------------------
## 6. Similar to analysis above, examine relationship between arrest intensity & criminal complaints
##  
##  a. read in "nypd_criminalcomplaints_2016.csv"
##      - this csv file shows # of criminal complaints for each subway station area
##      - source: https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
##
##  b. join stations data frame to subway station area crime data
##      - join on st_id
##      - exclude the stations with the 4 highest counts of criminal complaints
##        - why? bc they are in very close proximity to Brooklyn transit policing HQ,
##          and to Brooklyn criminal courts, so they don't face comparable
##          neighborhood policing conditions
##
##  c. examine relationship between arrest intensity & criminal complaints 
##      i. First, look at over all relationship (don't take nblack into account)
##      ii. Then, allow relationship to vary by nblack
##
##  NOTE: you don't need to follow *all* the same steps in questions 3-5.
##    for 6.c.i. and 6.c.ii:
##     - focus on showing your preferred plots to inform the relationship, 
##       along w/any additional data manipulation & evidence to support your decisions/interpretation/conclusions
##     - you'll want to explore the data before arriving at your preferred plots, 
##       DON'T show us everything you tried, just the analysis you settled on to support your answers
## -----------------------------------------------------------------------------

#6a.  
    st_crime <- read.csv("nypd_criminalcomplaints_2016.csv")

#6b.
    stations_wcrime <- inner_join(stations, st_crime) %>% 
      group_by(st_id, mta_name) %>% 
      arrange(desc(crimes)) %>% 
     #filter(ADD CODE)
      
    str(stations_wcrime$crimes)
  
#6c. 
  


  
## -----------------------------------------------------------------------------
## 7. interpret your findings with respect to subway fare evasion enforcement bias based on race
##    - any additional analysis you'd like to explore with the data at hand?
##    - are there any key limitations to the data/analysis affecting your
##       ability to assess enforcement bias based on race?
##    - any additional data you'd like to see that would strengthen your analysis/interpretation 
##    - for this question, try to be specific and avoid vaguely worded concerns
## -----------------------------------------------------------------------------
  
  