################################################################################
##
## [ PROJ ] < Lecture or Assignment Number >
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
##    a. inspect the data frame and data types for each column
##    b. use the mutate function to create a new column called type.fac = as.factor(type),
##        check if it worked by calling the str() function
##    c. include this column in a new data frame called covid.df1, 
##        use a pipe to exclude the original type column,
##        print the first 5 observations
##    d. inspect type.fac using the levels() function,
##        what package is the levels() function located in?
##    e. use the filter function to only include rows that represent "confirmed" cases,
##        store as a new object covid_confirmed.df,
##        print the first 5 observations,
##        confirm you did indeed drop rows representing deaths and recoveries
##    f. remove the covid.df1 object from memory using the rm() function
## -----------------------------------------------------------------------------

# somewhat unusually, we have a dataset saved in the old .rda file format
load("coronavirus.rda")

# 1a.
str(coronavirus) # note that we haven't discussed date variables yet
  # we can also inspect the data frame by double-clicking in the Environment tab

# 1b. 
mutate(coronavirus, type.fac = as.factor(type))
str(mutate(coronavirus, type.fac = as.factor(type)))
  
# 1c. 
covid.df1 <- mutate(coronavirus, type.fac = as.factor(type)) %>%
              select(-type)
head(covid.df1, n = 5)  

# 1d. hint: recall how we used the $ to refer to a column within a data frame
?levels
levels(covid.df1$type.fac)

# 1e.
covid_confirmed.df <- filter(covid.df1, type.fac == "confirmed")
head(covid_confirmed.df, n = 5)
summary(covid_confirmed.df$type.fac)
  # this worked, but it still thinks there are 2 other empty categories (levels)
  # here is a long way to fix that
  covid_confirmed.df <- mutate(covid_confirmed.df, type = as.character(type.fac)) %>%
                        mutate(covid_confirmed.df, type.fac = as.factor(type)) %>%
                        select(-type)

#1f.
rm(covid.df1)

## -----------------------------------------------------------------------------
## 2. Describe the corona.confirmed.df data frame
##    a. what is the unit of observation?
##    b. how many countries are observed?
##    c. how many days are observed?
## -----------------------------------------------------------------------------

# 2a.
str(covid_confirmed.df)
view(covid_confirmed.df)

# 2b. hint: we need to calculate a statistic for a given column of data
?summarise
summarise(covid_confirmed.df, n_distinct(country))

# 2c. 
summarise(covid_confirmed.df, n_distinct(date))


## -----------------------------------------------------------------------------
## 3. Let's look at confirmed case counts for the most recent day: 
##    a. find most recent day using the summarise function, assign to new object lastday
##    b. find most recent day using the arrange function
##    c. use the filter function to subset observations for the most recent day,
##        store in new data frame covid_confirmed_last.df,
##        confirm it worked
##    d. what was the max case count for the last day? top 5 countries by case count?
## -----------------------------------------------------------------------------

# 3a. 
summarise(covid_confirmed.df, max(date))

# 3b. 
  # approach 1
  arrange(covid_confirmed.df, desc(date))
  head(arrange(covid_confirmed.df, desc(date)) %>% select(date), n = 1)
  
  # approach 2
  arrange(covid_confirmed.df, desc(date)) %>% select(date) %>% head(n = 1)
  
# 3c. 
  # approach 1: hardcode last date
  covid_confirmed_last.df <- filter(covid_confirmed.df, date == "2020-07-18")
  summarise(covid_confirmed_last.df, min(date))

  # approach 2:
  # let's store the last day we obtained using summarise()
  # hint: inspect the results of summarise using typeof()
  # how do we access just the date? we can use base R syntax.
  lastday <- summarise(covid_confirmed.df, max(date))[,1]
  covid_confirmed_last.df <- filter(covid_confirmed.df, date == lastday)
  summarise(covid_confirmed_last.df, min(date))

# 3d.
summarise(covid_confirmed_last.df, max(cases))
select(covid_confirmed_last.df, country, cases) %>%
  arrange(desc(cases)) %>%
  head(n = 5)

## -----------------------------------------------------------------------------
## 4. Let's look at the stats for New Zealand: 
##    a. use the filter function to subset observations for New Zealand, 
##        assign to new data frame, covid_confirmed_nz.df,
##        check it worked
##    b. use summarise to find the daily mean, min and max for NZ over the whole pandemic,
##        name each statistic appropriately (i.e. name each column in the 1-row table of stats)
##    c. what was the average daily case count over past 30 days?
##        hint: use the arrange function to sort date in descending order,
##              refer to the lecture 2.1 on objects for syntax to refer to the first 30 rows,
##              
## -----------------------------------------------------------------------------
  
# 4a. 
covid_confirmed_nz.df <- filter(covid_confirmed.df, country == "New Zealand")
summarise(covid_confirmed_nz.df, n_distinct(country))

# 4b.
summarise(covid_confirmed_nz.df, nz_mean = mean(cases), nz_min = min(cases), nz_max = max(cases))

# 4c.
arrange(covid_confirmed_nz.df, desc(date))[1:30,] %>%
  summarise(nz_july_mean = mean(cases))


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------

