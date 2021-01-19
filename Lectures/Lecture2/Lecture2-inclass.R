################################################################################
##
## [ PROJ ] < Lecture2 >
## [ FILE ] < lecture2-inclass.r >
## [ AUTH ] < YOUR NAME>
## [ INIT ] < Date you started the file >
##
################################################################################


## ---------------------------
## libraries
## ---------------------------

library(tidyverse)

## ---------------------------
## directory paths
## ---------------------------

getwd()

## -----------------------------------------------------------------------------
## 1. load and inspect Coronavirus data
##    data source: 
##    https://www.ecdc.europa.eu/en/publications-data/data-national-14-day-notification-rate-covid-19
##
##    a. inspect the data frame and data types for each column
##        make sure to explore the indicator column
##
##    b. use the mutate function to create a new column:
##        ind.fac = as.factor(indicator),
##        check if it worked by calling the str() function
##
##    c. include this column in a new data frame called covid.temp1, 
##        use a pipe to exclude the columns for indicator, continent, country_code, source
##        print the first 5 observations
##
##    d. inspect ind.fac using the levels() function,
##        what package is the levels() function located in?
##
##    e. use filter() to only include rows representing "cases" and not "deaths",
##        store as a new object covid_cases,
##        print the first 5 observations,
##        note the column rate_14_day as the variable of interest (cases/100,000 pop)
##        confirm you did indeed drop rows representing deaths and recoveries
##
##    f. remove the covid.temp1 object from memory using the rm() function
## -----------------------------------------------------------------------------

#load the csv file and store as data frame called covid
  covid <- read.csv("ecdc_national_2021-01-14.csv")
  covid <- na.omit(covid) #remove all observations with NA values


#1a.
  str(covid) 
  View(covid)
  #we can also inspect the data frame by double-clicking in the Environment tab
  summary(covid$indicator) #not very useful for character variables
  
  #looks like we want to treat indicator as a categorical var (a factor, in R-speak!)
  #how do we inspect? #we can use the table function or we can coerce to a factor
  table(covid$indicator) 
  levels(covid$indicator) #returns NULL bc indicator is not a factor!
  levels(as.factor(covid$indicator)) #levels only works with factors
  summary(as.factor(covid$indicator))


#1b. 
  mutate(covid, ind.fac = as.factor(indicator)) #note we're not storing this result in memory
  str(mutate(covid, ind.fac = as.factor(indicator)))


#1c. 
  covid.temp1 <- covid %>%
    mutate(ind.fac = as.factor(indicator)) %>%
    select(-indicator, -continent, -source)
  
  #alternatively:
  covid.temp1 <- mutate(covid, ind.fac = as.factor(indicator)) %>%
    select(-indicator, -continent, -source)
  
  head(covid.temp1, n = 5)  
  
  #some helpful syntax for later: 
    #subset the first row of covid.df1
    covid.temp1[1,]
    #subset the cell in the first row, 4th column (i.e. first obs for year_week)
    covid.temp1[1,4]


#1d.
  levels(covid.temp1$ind.fac) 
  ?levels
  #note that levels is a base R function
  #so we can't refer to columns directly like tidyverse function
  #make sure you understand why this won't work: covid.df1 %>% levels(ind.fac)


#1e.
  covid_cases <- covid.temp1 %>% 
    filter(ind.fac == "cases")
  
  head(covid_cases, n = 5)
  
  summary(covid_cases$ind.fac)
  # this worked, but it still thinks there are 2 other empty categories (levels)
  # here is a long way to fix that
  covid_cases <- covid_cases %>%
    mutate(ind.char = as.character(ind.fac),
           ind.fac = as.factor(ind.char)) %>%
    select(-ind.char)
  #idea: convert the factor into a character first, then convert to factor again
  #when we convert the factor to a character, the original levels are lost
  #then R gets new levels based on the 1 remaining value ("confirmed")
  
  summary(covid_cases$ind.fac) # the # of obs for each level of ind.fac


#1f.
  rm(covid.temp1)
  
    #for reference, here is the code to remove all objects from the workspace:
    #rm(list = ls())


## -----------------------------------------------------------------------------
## 2. Describe the covid_cases data frame
##
##    a. what is the unit of observation? 
##       do you notice any observations that you think should be excluded?
##
##    b. how many entities are observed?
##
##    c. how many weeks are observed? earliest and latest year-week?
## -----------------------------------------------------------------------------

#2a.
  str(covid_cases)
  view(covid_cases)

  
#2b. hint: we need to calculate a statistic for a given column of data
  ?summarise
  summarise(covid_cases, n_distinct(country))
  
  #summarise is in the dplyr package which is part of the tidyverse
  covid_cases %>% 
    summarise(num_of_countries = n_distinct(country))
  
  #short way: use in your .rmd file to reference code to answer questions
  n_distinct(covid_cases$country)

  
#2c.
  #use summarise with multiple arguments, one for each statistic
  #try ?summarise to find the syntax for different summary statistics
  covid_cases %>% summarise(num_of_weeks = n_distinct(year_week),
                                   firstweek = min(year_week),
                                   lastweek = max(year_week))
  

## -----------------------------------------------------------------------------
## 3. Let's look at COVID case rate for the most recent year-week: 
##
##    a. find most recent year-week using the summarise() function, 
##        assign this date to a new object called lastweek
##
##    b. find most recent week using the arrange function instead of summarise
##
##    c. use the filter function to subset observations only for the most recent week
##        (don't hardcode a year-week to filter on, refer to lastweek object from a),
##        store in new data frame covid_cases_last,
##        confirm it worked
##
##    d. what was max 14 day case rate in any country for the most recent week? 
##       which country had the max rate?
##
##    e. list the top 10 countries by rate for the most recent week
##
##    f. how many entities had 0 reported cases for the most recent week?
## -----------------------------------------------------------------------------

#3a. 
  lastweek <- FILL IN CODE
  lastweek

#3b. 
  covid_cases %>% FILL IN CODE

#3c. 
  #HINT: your condition needs to refer to the last week.
  # you created lastweek as a data frame in part a,
  # so in your filter() call refer to the value from lastweek that you want to filter on
  # this requires subsetting the appropriate element from that data frame
  # (in this case just the 1st row of a 1-row data frame).
  # See Lecture2.1/Section 4.2 for examples of how to subset matrix/df elements
  covid_cases_last <- covid_cases %>% 
    FILL IN CODE

  #confirm this worked
    
  

#3d.
  

#3e. 


#3f.



## -----------------------------------------------------------------------------
## 4. Let's look at the 14-day case rate for Panama: 
##
##    a. use the filter function to subset observations for Panama, 
##        assign to new data frame, covid_cases_panama,
##        sort in descending date order
##        check it worked
##
##    b. use summarise to find mean, min & max for rate_14_day for Panama over all observations,
##        name each statistic appropriately (i.e. name each column in the 1-row table of stats)
##
##    c. what was the average weekly rate over past 10 weeks?
##        hint: see Lecture2.1/Section 4.2 for syntax to refer to first 10 rows
##          if you're having trouble, you can try using the row_number function
##
##    d. what was the average weekly rate over the first 10 weeks of data?
## -----------------------------------------------------------------------------
  
#4a. 

  
#4b.


#4c.


#4d.


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------

