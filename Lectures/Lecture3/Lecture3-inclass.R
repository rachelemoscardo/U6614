################################################################################
##
## [ PROJ ] Lecture 3: Subway Fare Evasion Arrests in Brooklyn
## [ FILE ] Lecture3-inclass.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Date you started the file >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
## Police can stop and ticket or  arrest people for subway fare evasion. 
## Is NYPD enforcement of subway fare evasion enforcement in Brooklyn racist?


## ---------------------------
## libraries
## ---------------------------

library(tidyverse)
library(forcats)
library(fastDummies)

## ---------------------------
## directory paths
## ---------------------------

getwd()
setwd("C:/Users/Harold Stolper/Google Drive/SIPA/R - Data Analysis/Fall 2020/Lectures/Lecture3")
## -----------------------------------------------------------------------------
## 1. load and inspect datasets
##
##    two datasets of administrative records from public defenders in Brooklyn
##      1. microdata_BDS_inclass.csv: Brooklyn Defender Services
##      2. microdata_LAS_inclass.csv: Legal Aid Society
##    each row is client data for an individual arrested for subway fare evasion
##    public defenders represent nearly everybody arrested for subway fare evasion
##    these files were provided by the respective orgs with no documentation
##
##  a. what is the unit of observation and representative population?
##  b. inspect and describe the coding of race/ethnicity information in each dataset?
## -----------------------------------------------------------------------------

arrests_bds <- read_csv("microdata_BDS_inclass.csv", na = "")
arrests_las <- read_csv("microdata_LAS_inclass.csv", na = "")

str(arrests_bds)
str(arrests_las)

# note that string variables are imported as characters by default (not factors)
# let's convert race and ethnicity to factors before we start cleaning these variables
arrests_bds <- arrests_bds %>% mutate(race = as.factor(race), 
                                      ethnicity = as.factor(ethnicity) )
summary(arrests_bds)

arrests_las <- arrests_las %>% mutate(race = as.factor(las_race_key), 
                                      ethnicity = as.factor(hispanic_flag) )
summary(arrests_las)

# 1a.

# 1b. 

## -----------------------------------------------------------------------------
## 2. clean BDS data: recode race and ethnicity variables and assign to arrests_bds_clean
## -----------------------------------------------------------------------------

# 2
  # BDS: race

  levels(arrests_bds$race)
  typeof(arrests_bds$race) #remember factors are stored as integers assigned to levels
  summary(arrests_bds$race)
  arrests_bds %>% count(race, sort = TRUE)
  arrests_bds %>% count(race, ethnicity, sort = FALSE)
  arrests_bds.clean_temp <- arrests_bds %>% 
    mutate(race_clean = recode(race, "0" = "NA", 
                                     "Unknown" = "NA", 
                                     "Am Indian" = "Other" ) )  %>% 
    mutate(race_clean = factor(race_clean, levels = c("Black", "White", "Asian/Pacific Islander", "Other")))

    # let's check we did this right  
    levels(arrests_bds.clean_temp$race_clean)
    arrests_bds.clean_temp %>% count(race_clean, sort = TRUE)
    arrests_bds.clean_temp %>% count(race_clean, race, sort = FALSE)
    table(arrests_bds.clean_temp$race_clean, arrests_bds.clean_temp$race, useNA = "always")
    
    # note: recode doesn't work well with pipes like other dplyr functions,
    #       instead we use the function as a part of a function argument


  # BDS: ethnicity
    levels(arrests_bds.clean_temp$ethnicity)
    table(arrests_bds.clean_temp$race_clean, arrests_bds.clean_temp$ethnicity, useNA = "always")
    arrests_bds.clean <- arrests_bds.clean_temp %>% 
      mutate(hispanic = recode(ethnicity, "0" = "NA", "Other" = "Non-Hispanic") ) %>%
      mutate(hispanic = factor(hispanic, levels = c("Hispanic", "Non-Hispanic"))) 

    # let's check we did this right  
    summary(arrests_bds.clean$hispanic)
    table(arrests_bds.clean$race_clean, arrests_bds.clean$hispanic, useNA = "always")
    
    # note: we used to separate pipe sequences to clean ethnicity & race, but could do in 1 pipe
    
    rm(arrests_bds.clean_temp)
    
## -----------------------------------------------------------------------------
## 3. BDS: create a single race_eth factor variable w/mutually exclusive categories:
##       - Black, Non-Hispanic White, Hispanic, Asian or Pacific Islander, Other, NA
##    Before you do this...
##    
##    In-class discussion questions: 
##      what cat do you want to use for Black Hispanic identity?
##      what is missing by using mutually exclusive categories?
##      was race coded by NYPD or client w/public defender input? we don't really know!
##    
##    a. Can you think of a better way to incorporate race into analysis of biased enforcement?
##    b. Create race_eth
## -----------------------------------------------------------------------------

# 3a. let's investigate a bit
  # here is a quick and easy way to show crosstabs using base R
  table(arrests_bds.clean$race_clean, arrests_bds.clean$hispanic, useNA = "always")
  prop.table(table(arrests_bds.clean$race_clean, arrests_bds.clean$hispanic), 2)  %>% round(2)
  str(arrests_bds.clean)  
  
# 3b.
  arrests_bds.clean <- arrests_bds.clean %>% 
    mutate(race_clean_char = as.character(race_clean)) %>% 
    mutate(hispanic_char = as.character(hispanic))     %>%
    mutate(race_eth = ifelse(hispanic_char == "Hispanic", hispanic_char, race_clean_char) ) %>%  
    mutate(race_eth = as.factor(recode(race_eth, "White" = "Non-Hispanic White") )  )%>%
    select(-race_clean_char, -hispanic_char)
    
  str(arrests_bds.clean)  
  table(arrests_bds.clean$race_eth, arrests_bds.clean$hispanic, useNA = "always")
  
## -----------------------------------------------------------------------------
## 4 a. Repeat 3 creating race_eth in the Legal Aid data (arrests_las) with the same coding
##      HINT: note that Hispanic identity is included in both las_race_key & hispanic_flag
##   b. Make sure you end up with a data frame with the following variable names,
##      and identical coding as inarrests_bds_clean:
##    - race_eth, age, male, dismissal (not in the BDS data), st_id, loc2
## -----------------------------------------------------------------------------

str(arrests_las)
table(arrests_las$las_race_key, arrests_las$hispanic_flag, useNA = "always")
arrests_las.clean <- arrests_las %>% 
  mutate(race_eth = recode(las_race_key, "Asian or Pacific Islander" = "Asian/Pacific Islander",
                                         "Unknown" = "NA",
                                         "Latino" = "Hispanic",
                                         "White" = "Non-Hispanic White")) %>% 
  mutate(race_eth = ifelse(hispanic_flag == "Y", "Hispanic", race_eth) ) %>% 
  mutate(race_eth = factor(race_eth, 
                           levels = c("Black", "Hispanic", "Non-Hispanic White", "Asian/Pacific Islander", "Other")))

# let's confirm we did this right
summary(arrests_las.clean$race_eth)
arrests_las.clean %>% count(race_eth, sort = TRUE)
table(arrests_las.clean$race_eth, arrests_las.clean$hispanic_flag, useNA = "always")

    
## -----------------------------------------------------------------------------
## 5. appending: stacking rows with rbind
##    a. create a field (pd) to identify the public defender data source (= "las" or "bds")
##    b. Append arrests_bds.clean and arrests_las.clean
##    c. use the nrow function to display the total number of arrests
##    d. inspect race_eth for accuracy/consistency, export new df as "arrests_all.csv"
##        (hint: use the write_csv function introduced in Lecture 3.1)
## -----------------------------------------------------------------------------

# 5a.
arrests_bds.clean <- arrests_bds.clean %>% mutate(pd = "bds")
arrests_las.clean <- arrests_las.clean %>% mutate(pd = "las")

# 5b.
arrests.clean <- plyr::rbind.fill(arrests_las.clean, arrests_bds.clean) %>%
  select(pd, race_eth, age, male, dismissal, st_id, loc2)
str(arrests.clean)
summary(arrests.clean)

# 5c. 

# 5d.
write_csv(arrests.clean, "arrests_all.csv") 

## -----------------------------------------------------------------------------
## 6. Grouping by race_eth
##    a. group arrests.clean by race_eth and store as its own df race_eth_counts
##          Note: we already obtained this information using the summary command,
##          but in some cases it might be useful to store this information to work with
##    b. compute average age, share male, and dimissal rate for each race/ethnicity category 
##          (store as race_eth_stats)
##        hint: make sure you tell R to ignore missing elements
##    c. what, if anything, do you think is interesting to note about
##        the distribution of age conditional on race, male given race, and dismissal by race
## -----------------------------------------------------------------------------

# 6a. 
race_eth_counts <- arrests.clean %>%
  group_by(race_eth) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
race_eth_counts
str(race_eth_counts)

# equivalent to using the count() function
arrests.clean %>% count(race_eth, sort = TRUE)

# if we want to exclude NAs
arrests.clean %>% 
  filter(!is.na(race_eth)) %>% 
  count(race_eth, sort = TRUE)


# 6b.
race_eth_stats <- arrests.clean %>%
  group_by(race_eth) %>%
  summarize(n = n(), 
            mean_age = mean(age, na.rm = TRUE),
            mean_male = mean(male, na.rm = TRUE))
race_eth_stats 

# 6c. 


## -----------------------------------------------------------------------------
## 7. grouping by subway station, with subway-station level statistics.
##      our data includes the following columns
##        - st_id: unique subway station identifier
##        - loc2: subway station name (recognized by google maps)
##   
##    a. let's use fastDummies::dummy_cols to create a dummy variable for each race_eth cat
##       this will come in handy later on.
##
##    b. create a new data frame w/station-level observations (arrests_stations) including the following:
##      - station name (given by loc2)
##      - st_id
##      - total number of arrests at each station
##      - total number of arrests for each race_eth category at each station
##      - share of arrests at each station that are Black and Hispanic
##      - sort in descending order of total number of arrests
##
##    c. show a date table (arrests_stations_top) with station-level observations including the following:
##      - station name,
##      - station arrest total
##      - sh_bh = share of arrests that are Black and Hispanic (excluding race_eth = NA from deonomintor)
##      - sorted in ascending order above Black and Hispanic arrest share
##      - only show for stations with at least 50 total arrests
##
##    d. briefly summarize any interesting findings about the distribution of race across stations
##      - hint: are there any high arrest stations (say, > 50 arrests total) with a high share of NHW arrests?
## -----------------------------------------------------------------------------

# 7a.
arrests.clean <- fastDummies::dummy_cols(arrests.clean, select_columns = "race_eth")
str(arrests.clean)
summary(arrests.clean)

# 7b.
# pretty easy to use group_by to get counts by station
arrests.clean %>%  
  group_by(loc2)  %>%
  summarize(st_id = first(st_id), n = n())  %>%
  arrange(desc(n))

# let's generate station-level counts for each race_eth group?
# approach A: summing dummy variables
arrests_stations <- arrests.clean %>%  
  group_by(loc2)    %>%
  summarize(st_id = first(st_id), 
            n = n(),
            n_black = sum(race_eth_Black, na.rm = TRUE), 
            n_hisp  = sum(race_eth_Hispanic, na.rm = TRUE),
            n_api   = sum(`race_eth_Asian/Pacific Islander`, na.rm = TRUE),
            n_oth   = sum(race_eth_Other, na.rm = TRUE) )   %>%
  arrange(desc(n))
arrests_stations

#7c. 
arrests_stations_top <- arrests.clean %>%  
  group_by(loc2)    %>%
  summarize(st_id = first(st_id), 
            n = n(),
            n_black = sum(race_eth_Black, na.rm = TRUE), 
            n_hisp  = sum(race_eth_Hispanic, na.rm = TRUE),
            n_api   = sum(`race_eth_Asian/Pacific Islander`, na.rm = TRUE),
            n_oth   = sum(race_eth_Other, na.rm = TRUE), 
            n_bh    = sum(race_eth_Black, race_eth_Hispanic, na.rm = TRUE),
            n_na    = sum(race_eth_NA))  %>%
  mutate(sh_bh = n_bh / (n - n_na)) %>% 
  filter(n >= 50) %>% 
  arrange(sh_bh)
arrests_stations_top

## -----------------------------------------------------------------------------
## 8. barplots 
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
  summarize(arrests = n(), st_arrests = first(st_arrests)) %>% 
  arrange(desc(st_arrests)) %>% 
  filter(st_arrests > 100)
arrests_stations_race

ggplot(arrests_stations_race, aes(x = reorder(loc2, -st_arrests), y = arrests, fill = race_eth)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
