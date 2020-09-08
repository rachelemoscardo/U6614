################################################################################
##
## [ PROJ ] Lecture1-inclass: Getting familiar with RStudio
## [ FILE ] Lecture1-inclass.r
## [ AUTH ] INSTRUCTOR FILE 
## [ INIT ] 9/8/2020
##
################################################################################

## -----------------------------------------------------------------------------
## 0. create an R project that is includes this R script (Lecture1-inclass.r)
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
#1. look around and get our bearings.
## -----------------------------------------------------------------------------

#get working directory
getwd()

## -----------------------------------------------------------------------------
## 2. install and load the gapminder package 
## -----------------------------------------------------------------------------

#first we have to install the package
install.packages("gapminder")

#we can see the list of packages under "Packages" on the bottom right panel of RStudio

#to load this package, type the following:
library(gapminder)
#another way to load is to click the checkbox beside the package name under "Packages"

## -----------------------------------------------------------------------------
## 3. Use base R functions to inspect a dataframe included w/ gapminder package 
##    (this exercise is based on STAT545 by Jenny Bryan)
## -----------------------------------------------------------------------------

#let's use some functions inspect the structure of the gapminder object
str(gapminder) #str (structure) is the function, gapminder is the argument
#notice that each variable has its own data type (Factor, int, num)

#how we access built-in help files/documentation... also try Google 
#(e.g. Stack Exchange or Stack Overflow)
?str

#the function class() tells us what class(es) an object is assigned to... more next week
class(gapminder)

#head is another function that shows us the first parts of an object
head(gapminder)

#note the presence of an optional argument n, which is the number of rows you want to see
?head

#let's try to see the first 10 rows
head(gapminder, n = 10)

#in order to view the full data frame, simply type View(data_frame_name) on the Console
#in this case, it is View(gapminder)

## -----------------------------------------------------------------------------
## 4. Use some base R functions to perform some very basic exploratory analysis
## -----------------------------------------------------------------------------

#let's use some base R functions to understand the basic structure of the data frame
names(gapminder) #get the column names
dim(gapminder) #get the number of rows and columns
ncol(gapminder) #get the number of columns (alternatively, dim(gapminder)[2])
nrow(gapminder) #get the number of rows (alternatively, dim(gapminder)[1])
names(gapminder)[3] #get the name of the third column

#note that indexing in R starts from 1, NOT 0

#we can assign the result of a function if we want to refer back to it:
num_of_vars <- ncol(gapminder) #assign the results of nrow to a new object called num_of_vars

num_of_vars #note that we need to type the object name again to view it

#get summary statistics for each variable
summary(gapminder)
#notice that the stats shown depend on the type of each column/variable
#for Factor, it lists how many times each level appears
#for int or num, it lists the first, second, third quartiles as well as min, max and mean

## let's plot the relationship of year (x) vs lifeExp (y) using base R
plot(lifeExp ~ year, gapminder) #the tilde (~) operator sets up a 'formula'

#alternative 1
plot((y = lifeExp) ~ (x = year), data = gapminder)

#alternative 2
plot(gapminder$year, gapminder$lifeExp)

#let's look at the documentation
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
levels(gapminder$continent) #returns the levels of a factor vector
nlevels(gapminder$continent) #returns the number of levels of a factor vector
#in this case, it's 5 since we have Africa, Americas, Asia, Europe and Oceania

#Note: a factor is just R's way of recognizing a categorical variable!

#here's one way to run a frequency table to check the number of obs per year
table(gapminder$year)

#let's assign this frequency table to a new object called freq_byyear
freq_byyear <- table(gapminder$year)

#then pass this object (a column of data!) as an argument to the barplot function
barplot(freq_byyear)
