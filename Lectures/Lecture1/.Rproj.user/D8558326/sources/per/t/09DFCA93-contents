# 1. Create an R project including R script


# 2. Look around and get our bearings.

#####
# 3. Install and load a package (gapminder)
install.packages("gapminder")
library(gapminder)


#####
# 4. Use base R functions to inspect a dataframe included w/gapminder package (also called “gapminder”)
#Note: this exercise is based on STAT 545 by Jenny Bryan (https://stat545.com/index.html)


## let's use some functions inspect the structure of the gapminder object
str(gapminder) 

## the class function tells us what classes an object is assigned to... more next week!
class(gapminder)

## head is another function that shows us the first parts of an object
head(gapminder)

## here's how we access built-in help files/documentation. also try google!
?str


#####
# 5. Use some functions to perform some basic analysis.

## let's understand what the basic characteristcs of the dataframe
names(gapminder)
dim(gapminder)
ncol(gapminder)

## we can assign the result of a function if we want to refer back to it:
num_of_vars <- ncol(gapminder)
num_of_vars #note that we need to type the object name again to view it

#Q: dim tells us the number of rows (observations) in the dataframe. 
#Use ?ncol to find that information using another function
nrow(gapminder)

## let's get summary statistics for each variable
summary(gapminder) 
  #notice that the statistics shown depend on the object type of each column/variable

## let's plot the relationship of gdpPercap (x) vs lifeExp (y) 
plot(lifeExp ~ year, gapminder) #note how the tilde (~) operator sets up a 'formula'
plot( (y = lifeExp) ~ (x = year), data = gapminder) 
  #this notation is clumsy, but reminds us of the arguments we're using in the plot function call
  ?plot

## let's apply some functions to specific variables 

### let's start with a continuous numeric variable (object type 'int')
### let's see how R uses the $ to refer to a single variable within a dataframe 
head(gapminder$lifeExp)
summary(gapminder$lifeExp)
hist(gapminder$lifeExp)

### next let's look at a factor variable -- let's use the results to learn what a factor is
class(gapminder$continent)
summary(gapminder$continent)
levels(gapminder$continent)
nlevels(gapminder$continent)
  #note that a factor is just a categorical variable

## here's how we do a frequency table to check the number of obs per continent
table(gapminder$continent)

## let's assign this frequency table to a new object called freq_bycon
freq_bycon <- table(gapminder$continent)
freq_bycon




