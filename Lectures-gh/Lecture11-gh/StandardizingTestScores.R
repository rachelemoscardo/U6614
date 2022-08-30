################################################################################
##
## [ PROJ ] Standardizing variables 
## [ FILE ] StandardizingTestScores.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < March 29, 2022 >
##
################################################################################

# This short excercise uses survey data from the Japanese goverment,
# for each of 47 prefectures from 2007-2021 (with some missing years).

# Our focus is on average scores (percent correct) on a standardized Japanese
# language test for elementary school students.


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)


## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

getwd()


## -----------------------------------------------------------------------------
## describe distribution of test scores by year and construct standardize measures
## -----------------------------------------------------------------------------

#read in data w/info on standardized Japanese test scores by prefecture-year
  scores <- read.csv("elementary_score.csv") %>% 
    select(wave, Prefecture, Japanese) %>% 
    filter(wave != 2011, wave != 2020) %>% #data missing in 2011 and 2020
    filter(wave < 2010) #let's focus on the first 3 years for this exercise

    #is this wide-form or long-form panel data?
  
  
#explore the distribution of test scores by year (wave)
  
  #get summary statistics to describe distribution
  scores %>% 
    group_by(wave) %>%
    summarise(wave_mean = mean(Japanese, na.rm = TRUE), 
              wave_sd = sd(Japanese, na.rm = TRUE))
  
  #visualize distribution
  scores %>% 
    ggplot() +
    aes(x = Japanese) +
    geom_histogram() + 
    facet_wrap(~ wave, nrow = 3)
  

#get *standardized* measure of test scores
#this changes the units to standard deviations of test scores
#what problem does this solve?
  
  #manually construct standardized measure
  scores_z <- scores %>% 
    group_by(wave) %>% 
    mutate(wave_mean = mean(Japanese),
           wave_sd = sd(Japanese),
           Japanese_z = (Japanese - wave_mean) / wave_sd )%>% 
    ungroup() %>% 
    mutate_if(is.numeric, round, digits = 2)
  
  #can also use the scale() function along with standardize()
  
  
  #visualize new, standardized distribution of scores by year
  scores_z %>% 
    ggplot() +
    aes(x = Japanese_z) +
    geom_histogram() + 
    facet_wrap(~ wave, nrow = 3)
  
  #what sort of visualization would help us see variation across all 14 years?
  
  