################################################################################
##
## [ PROJ ] Supplemental Example: Women in STEM
## [ FILE ] womeninstem.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Feb 14, 2022 >
##
################################################################################

## Research question:
##  - Do countries with more mandated maternal leave have a greater share of 
##    women graduating in STEM fields?
##  - A more interesting causal framing of that question:
##     Does maternal leave increase the women's representation in STEM fiels?

## Data source: World Bank's DataBank Gender Statistics
##  (https://databank.worldbank.org/source/gender-statistics)


##-------------------------
## load libraries
##-------------------------

# install.packages("ggrepel")
#install.packages("ggpmisc")

library(tidyverse)
library(ggrepel)
library(ggpmisc)

getwd()


##---------------------------
## 1. load & prep input data
##---------------------------

wbgender <- read_csv(FILL IN CODE)

#wait this data isn't "tidy"! diff vars for diff variables is... weird
  
#first, let's keep *rows* with information for 3 variables:
  #SE.TER.GRAD.FE.SI.ZS : 
    #Y: Female share of graduates from STEM programmes, tertiary (%)
  #SH.MMR.LEVE :
    #X1: Length of paid maternity leave (calendar days)
  #SP.UWT.TFRT :
    #X2: Unmet need for contraception (% of married women ages 15-49) 

  keepvars <- c(FILL IN CODE)
  stem <- wbgender %>% filter(`Series Code` %in% keepvars)
    #note: use backticks to refer to `Series Code` bc of space in col name 

  
##---------------------------
## 2. prepare analysis df
##---------------------------

## A. create 3 separate df's for each variable
##    - stem.fmshstemgrads, stem.dayspaidmatleave, stem.unmetcontr
##    - each df should be a subset of rows for each variable 
  

  

## B. create 'analysis variables' = max value across all years for each country
##    - hint: use mutate to create a new column
##    - hint: with mutate, use the pmax() function to get the max across rows
##    - pay attention to NAs
##    - only keep new cols for each analysis var, `Country Name`, `Country Code`
  
  stem.fmshstemgrads <- stem.fmshstemgrads %>%
    FILL IN CODE


  stem.unmetcontr <- stem.unmetcontr %>% 
    FILL IN CODE
  
  stem.dayspaidmatleave <- stem.dayspaidmatleave %>% 
    FILL IN CODE

  
  
## C. join 3 new df's together to get a single "tidy" dataframe
##    - including 3 analysis variables and `Country Name` and `Country Code`



  #think about sample selection issues! 
  #are missing observations for 3 analysis variables randomly distributed?

    
  
##---------------------------
## 3. exploratory analysis
##---------------------------

## A. explore relationship between days paid maternity leave & fem share of STEM grads

  

  
  #how would you describe the correlation?
    #what variation are we using? 

     
     
## B. could other country-level differences in part explain this relationship?
  
    

## C. what can we do to improve internal validity?
    #i.e. we at least want to identify a more informative correlation 
    # (if not a true causal effect)
    #don't answer with R code, think about research design!


  
  