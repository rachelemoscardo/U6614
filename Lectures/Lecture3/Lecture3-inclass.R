################################################################################
##
## [ PROJ ] Lecture 3: Subway Fare Evasion Arrests in Brooklyn
## [ FILE ] Lecture3-inclass.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Date you started the file >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
## Police can stop and ticket or arrest people for subway fare evasion. 
## Is there racial bias in their subway fare evasion enforcement in Brooklyn? 
## Week 3: What can we learn from microdata?

## About the microdata:

## The two public defender groups operating in Brooklyn, (Brooklyn Defenders and
## the Legal Aid Society), shared anonymized records for their clients who were 
## arrested for fare evasion in during 2016.


## ---------------------------
## 1. load libraries
## ---------------------------

library(tidyverse)
library(fastDummies)
#library(ggplot2) #included with Tidyverse
#library(forcats) #included with Tidyverse


#also confirm correct working directory
getwd()


## -----------------------------------------------------------------------------
## 2. load and inspect datasets using read_csv()
##
##    two datasets of administrative records from public defenders in Brooklyn
##      1. microdata_BDS_inclass.csv: Brooklyn Defender Services
##      2. microdata_LAS_inclass.csv: Legal Aid Society
##    each row is client data for an individual arrested for subway fare evasion
##      (the statutory charge is "Theft of Services")
##    do public defenders represent everybody arrested for subway fare evasion?
##    these files were provided by the respective orgs with no documentation
##
##  a. what is the unit of observation and representative population?
##  b. inspect and describe the coding of race/ethnicity information in each dataset?
## -----------------------------------------------------------------------------


#reading in blanks as NA
arrests_bds <- read_csv("microdata_BDS_inclass.csv", na = "")
arrests_las <- read_csv("microdata_LAS_inclass.csv", na = "")

str(arrests_bds)
str(arrests_las)


#note that string variables are imported as characters by default (not factors)
#let's convert race and ethnicity to factors to inspect before cleaning
  arrests_bds <- arrests_bds %>% mutate(race = as.factor(race), 
                                        ethnicity = as.factor(ethnicity) )
  arrests_las <- arrests_las %>% mutate(race = as.factor(las_race_key), 
                                        ethnicity = as.factor(hispanic_flag) )
#compare race coding
  summary(arrests_bds$race)
  summary(arrests_las$race)

#compare Hispanic/ethnicity coding
  summary(arrests_bds$ethnicity)
  summary(arrests_las$ethnicity)

  
#2a.

  
#2b.
  

## -----------------------------------------------------------------------------
## 3. clean BDS race and ethnicity data: 
##
##  recode race and ethnicity vars and assign to new df called arrests_bds_clean
##  generate 3 new columns for cleaned data:
##    a. race_clean
##    b. ethnicity_clean
##    c. create a single factor variable w/mutually exclusive groups (race_eth)
##       - Black, Non-Hispan White, Hispanic, Asian/Pacific Islander, Other, NA
##    
##      Before we do this...
##    
##      Questions to think about:
##        what cat do you want to use for Black Hispanic identity?
##        what is missing by using mutually exclusive categories?
## -----------------------------------------------------------------------------

#3a. BDS race

  #inspect
    levels(arrests_bds$race)
    typeof(arrests_bds$race) #remember factors are stored as integers corresponding to levels
    summary(arrests_bds$race)
    arrests_bds %>% count(race, sort = TRUE)
    arrests_bds %>% count(race, ethnicity, sort = FALSE)
    
    #a quick and easy way to show crosstabs using base R
      table(arrests_bds$race, arrests_bds$ethnicity, useNA = "always")
    
      
  #ok now let's recode in an internally consistent manner
    #recode 0 and Unknown into NA
    #recode Am Indian as Other because only 1 observation
    #assign to race_clean as a factor w/correct race groups
    arrests_bds.clean <- arrests_bds %>% 
      mutate(race_clean = recode(race, "0" = "NA", 
                                       "Unknown" = "NA", 
                                       "Am Indian" = "Other" ) )  %>% 
      mutate(race_clean = factor(race_clean, 
                                 levels = c("Black", "White", "Asian/Pacific Islander", "Other")))
  
      #validation: confirm the recode worked as intended
        levels(arrests_bds.clean$race_clean)
        arrests_bds.clean %>% count(race_clean, sort = TRUE)
        table(arrests_bds.clean$race, arrests_bds.clean$race_clean, 
              useNA = "always")
      
  #NOTE: recode doesn't work well with pipes like other dplyr functions,
  #      instead we use the function as a part of another function argument


#3b. BDS ethnicity
      
  #inspect
    levels(arrests_bds.clean$ethnicity)
    table(arrests_bds.clean$race_clean, arrests_bds.clean$ethnicity, useNA = "always")
  
  #ok now let's recode to Hispanic, Non-Hispanic, and NA
    arrests_bds.clean <- arrests_bds.clean %>% 
        mutate(hispanic = recode(ethnicity, "0" = "NA", "Other" = "Non-Hispanic") ) %>%
        mutate(hispanic = factor(hispanic, levels = c("Hispanic", "Non-Hispanic"))) 
  
    #validation: confirm the recode worked as intended
      summary(arrests_bds.clean$hispanic)
      table(arrests_bds.clean$race_clean, arrests_bds.clean$hispanic, useNA = "always")
      
#NOTE: we used separate pipes to clean ethnicity & race, could do in a single pipe
      

#3c. race_eth
      
  #let's investigate a bit
    table(arrests_bds.clean$race_clean, arrests_bds.clean$hispanic, useNA = "always")
    prop.table(table(arrests_bds.clean$race_clean, arrests_bds.clean$hispanic), 2)  %>% round(2)
  
  #generate race_eth column (as a factor) in steps
    arrests_bds.clean <- arrests_bds.clean %>% 
      mutate(race_clean_char = as.character(race_clean)) %>% #work with characters
      mutate(hispanic_char = as.character(hispanic))     %>% #work with characters
      mutate(race_eth = ifelse(hispanic_char == "Hispanic", 
                               hispanic_char, 
                               race_clean_char) ) %>%  
      mutate(race_eth = as.factor(recode(race_eth, "White" = "Non-Hispanic White"))) %>%
      select(-race_clean_char, -hispanic_char)
      
  #validation: inspect contents of race_eth
    str(arrests_bds.clean)  
    table(arrests_bds.clean$race_eth, arrests_bds.clean$hispanic, useNA = "always")
  
  
## -----------------------------------------------------------------------------
## 4a. Repeat step 2 for LAS:
##        - create race_eth in arrests_las with the same coding as for BDS
##      NOTE: Hispanic identity is included in both las_race_key & hispanic_flag
##    
##  b. Make sure you end up with a data frame with the following variable names,
##      and identical coding as in arrests_bds_clean:
##     - race_eth, age, male, dismissal (not in the BDS data), st_id, loc2
## -----------------------------------------------------------------------------

#4a.

    
#4b. validate


    
## -----------------------------------------------------------------------------
## 5. Append BDS and LAS microdata -- stack rows with rbind
##
##    a. create a column (pd) to identify PD data source (= "las" or "bds")
##
##    b. Append arrests_bds.clean and arrests_las.clean
##        - use rbind.fill from the plyr package
##          - bc of a conflict w/another package, use a "lazy load" of plyr:
##             plyr::rbind.fill()
##        - store combined data as new df called arrests_all
##        - inspect race_eth for accuracy/consistency
##
##    c. use the nrow function to display the total number of arrests
##
##    d. export new df as "arrests_all.csv", and save as rds file in Lecture4 folder for next week
##       HINT: use the write_csv function introduced in Lecture 3.1)
## -----------------------------------------------------------------------------

#5a.
  arrests_bds.clean <- arrests_bds.clean %>% mutate(pd = "bds")
  arrests_las.clean <- arrests_las.clean %>% mutate(pd = "las")

#5b.
  arrests.clean <- plyr::rbind.fill(arrests_las.clean, arrests_bds.clean) %>%
    mutate(pd = as.factor(pd),
           st_id = as.factor(st_id),
           loc2 = as.factor(loc2)) %>% #station/location info is not continuous
    select(pd, race_eth, age, male, dismissal, st_id, loc2)
  str(arrests.clean)
  summary(arrests.clean)

#5c. 
  nrow(arrests.clean)
  
#5d.
  write_csv(arrests.clean, "arrests_all.csv") 
  saveRDS(arrests_clean, "../Lecture4/arrests.clean.rds")

  
## -----------------------------------------------------------------------------
## 6. Descriptive statistics by race_eth (grouping)
##
##    a. group arrests.clean by race_eth,
##       show arrest counts for each race_eth category using summarise(),
##       store as its own df race_eth_counts
##        Note: we already obtained this information using the summary command,
##        but sometimes it may be useful to store this information to work with
##
##    b. show a table with the proportion of total arrests in each race_eth category
##
##    c. compute avg age, share male, and dimissal rate for each race_eth group
##        (store as race_eth_stats)
##        HINT: similar to (a) above, but specify different stats in summarise()
##        HINT: make sure you tell R to ignore missing elements
##
##    d. what, if anything, do you think is interesting to note about the
##       distribution of:
##        - age conditional on race
##        - male given race
##        - and dismissal by race
## -----------------------------------------------------------------------------

#6a. three approaches
  
  #tidyverse approach with summarise()
    race_eth_counts <- arrests.clean %>%
      group_by(race_eth) %>%
      summarise(n = n()) %>%
      arrange(desc(n))
    race_eth_counts
    str(race_eth_counts)

  #tidyverse with count()
    arrests.clean %>% count(race_eth, sort = TRUE)
  
  #base R with table()
    table(arrests.clean$race_eth)

  #if we want to exclude NAs
    arrests.clean %>% 
      filter(!is.na(race_eth)) %>% 
      count(race_eth, sort = TRUE)

#6b.
  prop.table(table(arrests.clean$race_eth)) %>% round(2)
  prop.table(table(arrests.clean$race_eth, useNA = "always")) %>% round(2)

  #a nicer looking version
  race_eth_prop <- prop.table(table(arrests.clean$race_eth, useNA = "always")) %>% 
    round(2) %>% 
    as.data.frame() %>% 
    arrange(desc(Freq)) %>% 
    rename(race_eth = Var1)
  race_eth_prop

  #exclude NAs
  prop.table(table(arrests.clean$race_eth)) %>% 
  round(2) %>% 
    as.data.frame() %>% 
    arrange(desc(Freq)) %>% 
    rename(race_eth = Var1)

  
#6c.


#6d. 


  
## -----------------------------------------------------------------------------
## 7. grouping by subway station, with subway-station level statistics.
##      our data includes the following columns
##        - st_id: unique subway station identifier
##        - loc2: subway station name (recognized by google maps)
##      for now: for the remainder of this Assignment, group by loc2
##   
##    a. use dummy_cols() in the fastDummies package to create dummies for each race_eth cat
##       this will come in handy shortly 
##
##    b. using group_by(), create a new data frame w/station-level observations,
##       store it as arrests_stations and including the following information:
##      - station name (given by loc2)
##      - st_id
##      - total number of arrests at each station
##      - total number of arrests for each race_eth category at each station
##      - sort in descending order of total number of arrests
##
##    c. modify table from part b w/station-level obs to add the following:
##      - sh_bh = share of arrests that are Black and Hispanic (excluding race_eth = NA from denominator)
##      - sort in ascending order of Black and Hispanic arrest share
##      - only show for stations with at least 50 total arrests
##      - store new table as a data frame called arrests_stations_top
##
##    d. briefly summarize any interesting findings about the distribution of race across stations
##      - hint: are there any high arrest stations w/a high share of NHW arrests?
## -----------------------------------------------------------------------------

#7a.
  arrests.clean <- dummy_cols(arrests.clean, select_columns = "race_eth")
  str(arrests.clean)
  summary(arrests.clean[,8:13])


#7b. use group_by to get counts by station
  arrests.clean %>%  
    group_by(loc2)  %>%
    summarise(st_id = first(st_id), n = n())  %>%
    arrange(desc(n))
  
  
  #let's generate station-level counts for each race_eth group?
  #approach: sum dummy variables
  arrests_stations <- arrests.clean %>%  
    group_by(loc2)    %>%
    summarise(st_id = first(st_id), 
              n = n(),
              n_black = sum(race_eth_Black, na.rm = TRUE), 
              n_hisp  = sum(race_eth_Hispanic, na.rm = TRUE),
              n_api   = sum(`race_eth_Asian/Pacific Islander`, na.rm = TRUE),
              n_oth   = sum(race_eth_Other, na.rm = TRUE) )   %>%
    arrange(desc(n))
  arrests_stations
  knitr::kable(head(arrests_stations)) #use kable for formatted tables


#7c. add/modify to approach from 7b


#7d.

## -----------------------------------------------------------------------------
## 8. OPTIONAL: barplots 
##    a. barplot of all arrests by race_eth category (proportions of all arrests, sorted)
##    b. same as a but exclude all observations with race_eth == NA
##    c. stacked barplot showing race_eth breakdown for top 10 stations by total arrest count
## -----------------------------------------------------------------------------

# 8a. 
ggplot(data = arrests.clean, aes(x = race_eth)) + geom_bar()
ggplot(data = arrests.clean, aes(x = fct_infreq(race_eth), y = ..prop.., group = 1)) + geom_bar() + 
  scale_y_continuous(labels = scales::percent_format()) 

# 8b.
arrests.clean.nomiss <- arrests.clean %>% filter(is.na(race_eth) == FALSE)
summary(arrests.clean.nomiss$race_eth)

ggplot(data = arrests.clean.nomiss, aes(x = race_eth)) + geom_bar()
ggplot(data = arrests.clean.nomiss, aes(x = fct_infreq(race_eth), y = ..prop.., group = 1)) + geom_bar() + 
  scale_y_continuous(labels = scales::percent_format())

# 8c.
arrests_stations_race <- arrests.clean %>%  
  group_by(loc2) %>% 
  mutate(st_arrests = n()) %>% 
  ungroup() %>% 
  group_by(loc2, race_eth)  %>%
  summarise(arrests = n(), st_arrests = first(st_arrests)) %>% 
  arrange(desc(st_arrests)) %>% 
  filter(st_arrests > 100)
arrests_stations_race

ggplot(arrests_stations_race, aes(x = reorder(loc2, -st_arrests), y = arrests, fill = race_eth)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 





