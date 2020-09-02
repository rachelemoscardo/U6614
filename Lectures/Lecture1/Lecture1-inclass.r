################################################################################
##
## [ PROJ ] Lecture1-inclass: Getting familiar with RStudio
## [ FILE ] Lecture1-inclass.r
## [ AUTH ] INSTRUCTOR FILE >
## [ INIT ] 9/28/2020
##
################################################################################

## -----------------------------------------------------------------------------
## 0. create an R project that is includes this R script (lecture1-inclass.r)
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
#1. look around and get our bearings.
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
## 2. install and load the gapminder package 
## -----------------------------------------------------------------------------

#first we have to install the package
  install.packages("gapminder")
  library(gapminder)


## -----------------------------------------------------------------------------
## 3. Use base R functions to inspect a dataframe included w/gapminder package 
##    (this exercise is based on STAT545 by Jenny Bryan)
## -----------------------------------------------------------------------------

#let's use some functions inspect the structure of the gapminder object
  str(gapminder) #str is the function, gapminder is the argument
    #notice that each variable has its own data type (Factor, int num)
  ?str #how we access built-in help files/documentation... also try google!

#class() tells us what classes an object is assigned to... more next week
  class(gapminder)

#head is another function that shows us the first parts of an object
  head(gapminder)
  ?head 
  head(gampinder, n = 10) #specify an optional second argument


## -----------------------------------------------------------------------------
## 4. Use some base R functions to perform some very basic exploratory analysis
## -----------------------------------------------------------------------------

#let's use base R functions to understand the basic structure of the data frame
  names(gapminder) 
  dim(gapminder)
  ncol(gapminder)

#we can assign the result of a function if we want to refer back to it:
  num_of_vars <- ncol(gapminder)
  num_of_vars #note that we need to type the object name again to view it

#dim tells us the number of rows (observations) in the dataframe. 
#run ?ncol to find a way to get that information using another function
  ?ncol
  nrow(gapminder)

#let's get summary statistics for each variable
  summary(gapminder) 
    #notice that the stats shown depend on the type of each column/variable

## let's plot the relationship of gdpPercap (x) vs lifeExp (y) 
  plot(lifeExp ~ year, gapminder) #the tilde (~) operator sets up a 'formula'
  plot((y = lifeExp) ~ (x = year), data = gapminder) 
   #this notation is clumsy, but reminds us of the arguments we're using above
    ?plot

#let's apply some functions to specific variables (i.e. columns of data) 
#start with a continuous numeric variable (object type 'int')
#let's see how R uses $ to refer to a variable in a dataframe (subsetting)
  head(gapminder$lifeExp)
  summary(gapminder$lifeExp)
  hist(gapminder$lifeExp)

#next let's look at a factor variable
#what is a factor? let's look at some base R functions to figure it out
  str(gapminder)
  class(gapminder$continent)
  summary(gapminder$continent)
  levels(gapminder$continent)
  nlevels(gapminder$continent)
    #a factor is just R's way of recognizing a categorical variable!

#here's one way to run a frequency table to check the number of obs per year
  table(gapminder$year)

#let's assign this frequency table to a new object called freq_byyear
  freq_byyear <- table(gapminder$year)
  
#then pass this object (a column of data!) as an arg to the barplot function
  barplot(freq_byyear)




