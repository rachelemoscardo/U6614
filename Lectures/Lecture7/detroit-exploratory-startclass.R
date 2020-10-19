################################################################################
##
## [ PROJ ] Lecture 7: Water shutoffs, race, and health in Detroit (Part 1)
## [ FILE ] detroit-exploratory-startclass.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Oct 20, 2020 >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
##
## 177,000 Detroit homes have had their water shut off for non-payment since 2010
##
## Today: who is most affected by this problem?
##
## Next week: what are the associated public health impacts?


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

#library(foreign)     #note only imports .dta files up thru Stata 12
library(readstata13)  #imports .dta files from Stata 13 thru 15
library(tidyverse)
library(lubridate)
library(splitstackshape)
library(fastDummies)
library(spatstat)
library(plm)
library(weights)


## -----------------------------------------------------------------------------
## directory paths: make sure all input data is saved in "../Data"
## -----------------------------------------------------------------------------

getwd()




## -----------------------------------------------------------------------------
## Get demographic data
## -----------------------------------------------------------------------------


  

## -----------------------------------------------------------------------------
## Get service interruption (SI) data - shutoff records (microdata)
## -----------------------------------------------------------------------------
  




## -----------------------------------------------------------------------------
## Join SI & demographic data: construct tract-month panel & tract-level totals
## -----------------------------------------------------------------------------





## -----------------------------------------------------------------------------
## Analyze relationships between tract-level income, race and SI per capita
## -----------------------------------------------------------------------------




