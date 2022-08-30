################################################################################
##
## [ PROJ ] Supplemental Example: Women in STEM
## [ FILE ] womeninstem.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Feb 22, 2022 >
##
################################################################################

## Research question:
##  - Do countries with more mandated maternal leave have a greater share of 
##    women graduating in STEM fields?
##  - A more interesting causal framing of that question:
##     Does maternal leave increase the women's representation in STEM fields?

## Data source: World Bank's DataBank Gender Statistics
##  (https://databank.worldbank.org/source/gender-statistics)


##-------------------------
## load libraries
##-------------------------

#install.packages("ggrepel")
#install.packages("ggpmisc")

library(tidyverse)
library(ggrepel)
library(ggpmisc)

getwd()


##---------------------------
## 1. load & prep input data
##---------------------------

wbgender <- read_csv("worldbank-genderstats.csv") #ADD ARGUMENT TO ADDRESS NAs


#wait this data isn't "tidy"! 
#each row is not its own observation! there are diff rows for diff vars
  
#here are the 3 input variables we want to work with
  #SE.TER.GRAD.FE.SI.ZS : 
    #Y: Female share of graduates from STEM programmes, tertiary (%)
  #SH.MMR.LEVE :
    #X1: Length of paid maternity leave (calendar days)
  #SP.UWT.TFRT :
    #X2: Unmet need for contraception (% of married women ages 15-49) 


#first, let's keep *rows* with information for 3 variables:
  keepvars <- c(FILL IN CODE)
  stem <- wbgender %>% filter(`Series Code` %in% keepvars)
    #NOTE: use backticks to refer to `Series Code` bc of space in col name 
    #NOTE: the %in% operator checks if two vectors contain overlapping values 

  
##---------------------------
## 2. prepare analysis df
##---------------------------

## A. create 3 separate df's for each variable
##  - stem.fmshstemgrads, stem.dayspaidmatleave, stem.unmetcontr
##  - each df should be a subset of rows for each variable 
##  - in each df, look at the distribution for each var in each year - any concerns?

  stem.fmshstemgrads <- FILL IN CODE   

  stem.dayspaidmatleave <- FILL IN CODE
  
  stem.unmetcontr <- FILL IN CODE
  

## B. for each variable, create 'analysis variable' = mean value across all years for each country
##  - 1. start by reshaping data from wide to long form in each df using pivot_longer
##    - new long form df should have 1 obs for every country-year combination
##    - i.e. reshape stem.fmshstemgrads into new df stem.fmshstemgrads_long, etc.
##  - 2. next use group_by + summarise to generate aggregated stats for each group
##    - new df should have 3 columns: `Country Name`, `Country Code`, fmshstemgrads
##    - repeat for each input variable to end up with 3 data frames
  
  stem.fmshstemgrads_long <- stem.fmshstemgrads %>% 
    pivot_longer(cols = FILL IN ARGUMENT FOR COLS TO RESHAPE,
                 names_to = FILL IN ARGUMENT FOR NEW COL NAME W/TIME INFO,
                 values_to = "value" ) #ARGUMENT FOR NEW COL NAME W/VARIABLE VALUES
  #here's a basic pivot_longer example: 
  #https://statisticsglobe.com/pivot_longer-and-pivot_wider-functions-in-r

  
  #once you get the reshape down above, 
  #then extend the pipe w/group_by + summarize to get aggregate observations
  stem.fmshstemgrads_long <- stem.fmshstemgrads %>% 
    pivot_longer(cols = FILL IN ARGUMENT FOR COLS TO RESHAPE,
                 names_to = FILL IN ARGUMENT FOR NEW COL NAME W/TIME INFO,
                 values_to = "value") %>% 
    group_by(FILL IN ARGS) %>% 
    summarise(fmshstemgrads = FILL IN CODE TO GET COUNTRY-LEVEL STATISTICS)
  
  
  stem.unmetcontr_long <- stem.unmetcontr %>% 
    FILL IN CODE
  
  
  stem.dayspaidmatleave_long <- stem.dayspaidmatleave %>% 
    FILL IN CODE

  
  
## C. join 3 new df's together to get a single "tidy" dataframe
##  - include 3 analysis variables and `Country Name` and `Country Code`
##  - how many countries have non-missing values for all 3 vars?

  stem.cross <- FILL IN CODE 

  #think about sample selection issues! 
  #are missing observations for original variables randomly distributed?

    
  
##---------------------------
## 3. exploratory analysis
##---------------------------

## stem.cross has issues w/measurement error and non-random sample selection
## these issues limit the usefulness of this data for credible project work
## but we'll proceed a bit further for this in-class exercise 
  
  
## A. explore relationship between days paid maternity leave & fem share of STEM grads

  

  
  #how would you describe the correlation?
    #what variation are we using? 

     
     
## B. could other country-level differences in part explain this relationship?
##    - what can you check in the data? 
    

## C. what can we do to improve internal validity?
    #i.e. we at least want to identify a more informative correlation 
    # (if not a true causal effect)
    #don't answer with R code, think about research design!


  
  