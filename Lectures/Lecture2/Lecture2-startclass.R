################################################################################
##
## [ PROJ ] < Lecture2 >
## [ FILE ] < lecture2-startclass.r >
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


#1b. 



#1c. 


  #some helpful syntax for later: 
    #subsetting the first row of covid.df1


    #subsetting the first cell of covid.df1 (i.e. first row for date column)
 

#1d.



#1e.


#1f.


  
  #for now, let's just assign unique values to country (country-province)
  covid_confirmed.df <- covid_confirmed.df %>% 
    mutate(country = if_else(province == "", 
                             country, 
                             paste(country, "-", province)))
    #henceforth: country = country or administrative entity reporting case data

  
#1g.



## -----------------------------------------------------------------------------
## 2. Describe the corona.confirmed.df data frame
##
##    a. what is the unit of observation? (note any inconsistencies/questions)
##
##    b. how many countries (or administrative entities reporting data) are observed?
##
##    c. how many days are observed? earliest and latest date?
## -----------------------------------------------------------------------------

# 2a.



# 2b. hint: we need to calculate a statistic for a given column of data

  
  #short way: you can use in your .rmd file as inline code to answer questions
  

# 2c. 



## -----------------------------------------------------------------------------
## 3. Let's look at confirmed case counts for the most recent day: 
##
##    - find most recent date using the summarise() function, 
##        assign this date to a new object called lastday
##
##    - find most recent day using the arrange function instead of summarise
##
##    a. use the filter function to subset observations for the most recent day
##        (don't hardcode a date to filter on, refer to lastday object from a),
##        store in new data frame covid_confirmed_last.df,
##        confirm it worked
##
##    b. what was max case count in any country for the most recent day? 
##
##    c. list the top 5 countries by case count for the most recent day
##
##    d. how many countries had 0 cases for the most recent day?
## -----------------------------------------------------------------------------

#-

  
#-

  
#3a. 
#HINT: your condition needs to refer to the last day.
# if you created lastday as a data frame in part a,
#   then in your filter() call you need to subset the element of data 
#   that includes the date information (in this case just the first row).
# See Lecture2.1/Section 4.2 for examples of how to subset matrix/df elements


#confirm this worked


#3b.


#3c. 



#3d.



## -----------------------------------------------------------------------------
## 4. Let's look at the case counts for Oman: 
##
##    a. use the filter function to subset observations for Oman, 
##        assign to new data frame, covid_confirmed_oman.df,
##        sort in descending date order
##        check it worked
##
##    b. use summarise() to find the daily mean, min and max for Oman
##        over the duration of the pandemic,
##        name each statistic appropriately 
##        (i.e. name each column in the 1-row table of stats)
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

