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
##    data source: https://github.com/RamiKrispin/coronavirus/tree/master/data
##
##    a. inspect the data frame and data types for each column
##        make sure to explore the type column
##
##    b. use the mutate function to create a new column:
##        type.fac = as.factor(type),
##        check if it worked by calling the str() function
##
##    c. include this column in a new data frame called covid.df1, 
##        use a pipe to exclude the original type column,
##        print the first 5 observations
##
##    d. inspect type.fac using the levels() function,
##        what package is the levels() function located in?
##
##    e. use filter() to only include rows representing "confirmed" cases,
##        store as a new object covid_confirmed.df,
##        print the first 5 observations,
##        confirm you did indeed drop rows representing deaths and recoveries
##
##    f. take a look at the province column:
##        are there any data cleaning actions we should take?
##
##    g. remove the covid.df1 object from memory using the rm() function
## -----------------------------------------------------------------------------

#somewhat unusually, we have a dataset saved in the old .rda file format
  load("coronavirus.rda")


#1a.
  str(coronavirus) 
    #note that we haven't discussed date variables yet
    #we can also inspect the data frame by double-clicking in the Environment tab
  summary(coronavirus$type)
  table(coronavirus$type)
  ?levels
  levels(coronavirus$type) #returns NULL bc type is not a factor!

#1b. 
  mutate(coronavirus, type.fac = as.factor(type)) #here, we are not storing this result in the memory
  str(mutate(coronavirus, type.fac = as.factor(type)))


#1c. 
  covid.df1 <- mutate(coronavirus, type.fac = as.factor(type)) %>%
                select(-type) #removing the type column
  
  #alternatively
  covid.df1 <- coronavirus %>%
    mutate(type.fac = as.factor(type)) %>%
    select(-type)
  
  head(covid.df1, n = 5)  
  
  #some helpful syntax for later: 
    #subsetting the first row of covid.df1
      covid.df1[1,]
    #subsetting the first cell of covid.df1 (i.e. first row for date column)
      covid.df1[1,1]
      

#1d.
  levels(covid.df1$type.fac)


#1e.
  covid_confirmed.df <- filter(covid.df1, type.fac == "confirmed")
  
  #alternatively,
  covid_confirmed.df <- covid.df1 %>%
    filter(type.fac == "confirmed")
  
  head(covid_confirmed.df, n = 5)
  summary(covid_confirmed.df$type.fac)
    # this worked, but it still thinks there are 2 other empty categories (levels)
    # here is a long way to fix that
    covid_confirmed.df <- covid_confirmed.df %>%
      mutate(type = as.character(type.fac),
             type.fac = as.factor(type)) %>%
      select(-type)
    #idea: convert the factor into a character first, then bring it to factor again
    #when we convert the factor into a character, the information about the levels with 
    #no observation is lost
    
    summary(covid_confirmed.df$type.fac)

#1f.
  table(covid_confirmed.df$province)
  
  #some examples:
  covid_confirmed.df %>% filter(country == "China", date == "2020-01-22")  
  covid_confirmed.df %>% filter(country == "Denmark", date == "2020-01-22")  
  
  #for now, let's just assign unique values to country (Country-Province)
  covid_confirmed.df <- covid_confirmed.df %>% 
    mutate(country = if_else(province == "", country, paste(country, "-", province)))
    #henceforth: country = country or administrative entity reporting case data

#1g.
  rm(covid.df1)

#for reference, here is the code to remove all objects from the workspace:
#rm(list = ls())


## -----------------------------------------------------------------------------
## 2. Describe the corona.confirmed.df data frame
##
##    a. what is the unit of observation? (note any inconsistencies/questions)
##
##    b. how many countries (or administrative entities reporting data) are observed?
##
##    c. how many days are observed? earliest and latest date?
## -----------------------------------------------------------------------------

#2a.
  str(covid_confirmed.df)
  view(covid_confirmed.df)


#2b. hint: we need to calculate a statistic for a given column of data
  ?summarise
  
  summarise(covid_confirmed.df, n_distinct(country))
  
  covid_confirmed.df %>% 
    summarise(num_of_countries = n_distinct(country))
  
  #short way: use in your .rmd file to reference code to answer questions
   n_distinct(covid_confirmed.df$country)

#2c. 
  covid_confirmed.df %>% summarise(num_of_days = n_distinct(date),
                                   firstday = min(date),
                                   latday = max(date))


## -----------------------------------------------------------------------------
## 3. Let's look at confirmed case counts for the most recent day: 
##
##    a. find most recent date using the summarise() function, 
##        assign this date to a new object called lastday
##
##    b. find most recent day using the arrange function instead of summarise
##
##    c. use the filter function to subset observations for the most recent day
##        (don't hardcode a date to filter on, refer to lastday object from a),
##        store in new data frame covid_confirmed_last.df,
##        confirm it worked
##
##    d. what was max case count in any country for the most recent day? 
##
##    e. list the top 5 countries by case count for the most recent day
##
##    f. how many countries had 0 cases for the most recent day?
## -----------------------------------------------------------------------------

#3a. 


#3b. 


#3c. 
  #HINT: your condition needs to refer to the last day.
  # if you created lastday as a data frame in part a,
  # then in your filter() call you need to subset the element of data 
  # that includes the date information (in thie case just the first row).
  # See Lecture2.1/Section 4.2 for examples of how to subset matrix/df elements



  #confirm this worked



#3d.


#3e. 



#3f.



## -----------------------------------------------------------------------------
## 4. Let's look at the case counts for Oman: 
##
##    a. use the filter function to subset observations for Oman, 
##        assign to new data frame, covid_confirmed_oman.df,
##        sort in descending date order
##        check it worked
##
##    b. use summarise to find the daily mean, min and max for Oman over the whole pandemic,
##        name each statistic appropriately (i.e. name each column in the 1-row table of stats)
##
##    c. what was the average daily case count over past 30 days?
##        hint: see Lecture2.1/Section 4.2 for syntax to refer to first 30 rows
##          if you're having trouble, you can try using the row_number function
##
##    d. what was the average daily case count over the first 30 days of data?
## -----------------------------------------------------------------------------
  
#4a. 


#4b.


#4c.


#4d.



## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------

