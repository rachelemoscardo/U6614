################################################################################
##
## [ PROJ ] Lecture 4: Subway Fare Evasion Arrests in Brooklyn
## [ FILE ] Lecture4-inclass.r
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
library(weights)
library(lmtest)
library(sandwich)

## ---------------------------
## directory paths
## ---------------------------

getwd()

## -----------------------------------------------------------------------------
## 1. a. load arrest microdata from last week (arrests_all.csv) with all strings as factors
##    b. collapse to station-level observations including the following information:
##        - st_id, loc2, arrests_all (arrest count), age_mean, dismissal_mean
##    c. plot histogram of arrests
##    d. plot histogram of dismissal rates for stations w/ >=40 arrests,
##        any interesting observations about dismissal rates across stations?
## -----------------------------------------------------------------------------

# 1a.
arrests_all <- read.csv("arrests_all.csv", stringsAsFactors = TRUE)

# 1b.
st_arrests <- arrests_all %>% 
  group_by(st_id, loc2) %>% 
  summarise(arrests_all = n(),
            age_mean = mean(age, na.rm = TRUE),
            dismissal_mean = mean(dismissal, na.rm = TRUE) ) %>% 
  arrange(desc(arrests_all))
head(st_arrests, n = 10)

# 1c.
ggplot(data = st_arrests, aes(x = arrests_all)) + geom_histogram()


# 1d.
st_arrests_gt50 <- st_arrests %>% filter(arrests_all >= 50)
ggplot(st_arrests_gt50, aes(x = dismissal_mean)) + geom_histogram()

  # top high-arrest stations by dismissal rate
  st_arrests %>% 
    arrange(desc(dismissal_mean)) %>% 
    filter(arrests_all > 50) %>% 
    head(st_arrests, n = 10)
  
  # bottom high-arrest stations by dismissal rate
  st_arrests %>% 
    arrange(dismissal_mean) %>% 
    filter(arrests_all > 50) %>% 
    head(st_arrests, n = 10)


## -----------------------------------------------------------------------------
## 2. a. import other station-level datasets and inspect
##    b. merge both files to st_arrests and inspect merges (call new dr st_merged),
##        group st_merged by st_id and mta_name
##    c. print top 10 stations by arrest counts w/povrt_all and shareblack cols
## -----------------------------------------------------------------------------

# 2a. 
st_poverty <- read.csv("station_povdataclean_2016.csv", stringsAsFactors = TRUE)
  # poverty data: 
  #   each unit represents a "subway station area", defined as follows:
  #     all census tracts w/geometric center within .5km of a station (see map)
  #   each observation is a subway station area with a unique identifier (st_id)
  #   povrt_all_2016: share of adults in a subway station area living below federal poverty level
  #   shareblack: share of adults in subway station area who identify as Black
  #   nblack: dummy variable = 1 if shareblack >= 50%, 0 otherwise
st_ridership <- read.csv("Subway Ridership by Station - BK.csv", stringsAsFactors = TRUE)
  # ridership data:
  #   each observation is a subway station (area) with a unique identifier (st_id)
  #   includes annual # of MetroCard swipes at each station (from the MTA) for 2011-16

str(st_poverty)
str(st_ridership)

# 2b.

drop_vars <- c("swipes2011", "swipes2012", "swipes2013", "swipes2014", "swipes2015")
st_merged_grouped <- inner_join(st_poverty, st_ridership) %>%
  inner_join(st_arrests) %>% 
  select(!drop_vars) %>% 
  group_by(st_id, mta_name) %>% 
  as_tibble()
str(st_merged_grouped)
summary(st_merged_grouped)
  #Note: 157 obs in merged df w/no NAs (except for missing demographics) - inner join worked!

# 2c.
st_merged_grouped %>% 
  arrange(desc(arrests_all)) %>% 
  select(st_id, mta_name, arrests_all, shareblack, povrt_all_2016) %>% 
  head(n = 10)

## -----------------------------------------------------------------------------
## 3. arrest intensity and poverty 
##    a. exclude coney island station from the analysis sample
##       create new variable measuring enforcement intensity:
##         arrperswipe_2016 =  arrests per 100,000 swipes
##       create new dummy variable indicating high poverty station area:
##         highpov = 1 if pov rate is > median pov rate across stations
##       assign results to new df (stations)
##    b. investigate arrests intensity vs poverty rates
##       - plot arrperswipe vs povrt_all_2016
##       - investigate linear and quadratic model fit
##       - report diff in mean arrest intensity between high/low pov areas, is diff significant?
## ---------------------------------------------------------------------------

# 3a.
stations_grouped <- st_merged_grouped %>% 
  filter(st_id != 66) %>% 
  mutate(arrperswipe = arrests_all / swipes2016 * 100000) %>% # create station swipe intensity ("rate")
  mutate(highpov = 
          as.numeric(povrt_all_2016 >= median(povrt_all_2016) ) ) 
          # note we can directly test condition as a logical comparison, converted to numeric
  #check highpov split
  table(stations_grouped$highpov, stations_grouped$nblack )


  # now check top 10 stations by arrest intensity
  stations_grouped %>% 
    arrange(desc(arrperswipe)) %>% 
    select(st_id, mta_name, arrperswipe, arrests_all, shareblack, povrt_all_2016, highpov) %>% 
    head(n = 10)
  
# 3b. 
    ggplot(stations_grouped, #specify which dataframe to use
         aes(x = povrt_all_2016, y = arrperswipe)) + #specify which columns to use
    geom_point() + #specify the type of plot
    ggtitle('Scatterplot of arrest rate vs. poverty rate') + #add title
    labs(x = 'poverty rate', y = 'arrest rate') #change axis labels

  #fit linear OLS model (arrest rate vs. poverty rate)
  ols1l <- lm(arrperswipe ~ povrt_all_2016, data = stations_grouped)
    #note: x ~ y is the syntax for an R "formula", we'll use to specify models
  summary(ols1l) #get summary of the model
  coeftest(ols1l, vcov = vcovHC(ols1l, type="HC1")) #get robust SEs
  
  #add linear prediction line to scatter plot
  ggplot(stations_grouped,
         aes(x = povrt_all_2016, y = arrperswipe)) + 
    geom_point() + 
    ggtitle('Linear regression fit') + 
    labs(x = 'poverty rate', y = 'arrest rate') + 
    geom_smooth(method = 'lm', formula = y ~ x) #add regression line
  
  
  #fit quadratic OLS model (arrest rate vs. poverty rate)
  ols1q <- lm(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2),
             data = stations_grouped) #include quadratic term
  summary(ols1q) #get summary of the model
  coeftest(ols1q, vcov = vcovHC(ols1q, type="HC1")) #get robust SEs
  

  #add quadratic prediction line to scatter plot
  ggplot(stations_grouped,
         aes(x = povrt_all_2016, y = arrperswipe)) + 
    geom_point() + 
    ggtitle('Linear regression fit') + 
    labs(x = 'poverty rate', y = 'arrest rate') + 
    geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) #add regression line

  # difference in means (unweighted)
  stations_grouped %>% 
    ungroup() %>% 
    group_by(highpov) %>% 
    summarise(n = n(),
              mean_pov = mean(povrt_all_2016),
              mean_arrper = mean(arrperswipe))

    #inference with t.test command and unequal variance
    t.test(arrperswipe ~ highpov, data = stations_grouped, var.equal = FALSE)

    #inference with bivariate regression and robust SE coefficient test
    dim1 <- lm(arrperswipe ~ highpov, data = stations_grouped)
    summary(dim1) #get summary of the model
    coeftest(dim1, vcov = vcovHC(dim1, type="HC1")) #get robust SEs
    
    
  # difference in means (weighted)
  stations_grouped %>% 
    ungroup() %>% 
    group_by(highpov) %>% 
    summarise(n = n(),
              mean_pov = weighted.mean(povrt_all_2016, swipes2016),
              mean_arrper = weighted.mean(arrperswipe, swipes2016))
    #?weighted.mean
  
  stations_grouped_highpov <- stations_grouped %>% filter(highpov == 1)
  stations_grouped_lowpov  <- stations_grouped %>% filter(highpov == 0)
  wtd.t.test(stations_grouped_highpov$arrperswipe, stations_grouped_lowpov$arrperswipe, 
             weight = stations_grouped_highpov$swipes2016, 
             weighty = stations_grouped_lowpov$swipes2016)
    #?wtd.t.test

## -----------------------------------------------------------------------------
## 4. arrest intensity and poverty by Black vs non-Black station area (nblack)
##    a. difference in means table: arrests per swipe by highpov vs nblack
##    b. scatterplot of arrperswipe vs povrt_all by nblack
##       add linear fit
##       add quadratic regression fit
##    c. which model do you prefer, linear or quadratic?
## -----------------------------------------------------------------------------
  
# 4a.
  t1_arrper <- with(stations_grouped, 
                    tapply(arrperswipe, 
                           list("High Poverty" = highpov, "Predominantly Black" = nblack), 
                            mean) )
  
  t1_arrper <- with(stations_grouped, 
                    tapply(arrperswipe, 
                           list("High Poverty" = highpov, "Predominantly Black" = nblack), 
                           mean) )
  t1_arrper_wtd <-
    tapply(stations_grouped$arrperswipe * stations_grouped$swipes2016,
           list(stations_grouped$highpov, stations_grouped$nblack), 
           sum) / 
    tapply(stations_grouped$swipes2016,
           list(stations_grouped$highpov, stations_grouped$nblack), 
           sum)  
    
  t1_n <- table(stations_grouped$highpov, stations_grouped$nblack )
  
  t1_arrper
  t1_arrper_wtd
  t1_povrt
  t1_n
  
# 4b.
  #scatterplot by nblack
  ggplot(stations_grouped, aes(x = povrt_all_2016, y = arrperswipe, color=as.factor(nblack))) +
    geom_point() 
  
  # add linear plot
  ggplot(stations_grouped, aes(x = povrt_all_2016, y = arrperswipe, color=as.factor(nblack))) +
    geom_point()  +
    # Add quadratic curve
    geom_smooth(method = 'lm', formula = y ~ x) + #add regression line
    # Add axis labels and title
    ylab("Arrests relative to ridership") + xlab("Station area poverty rate") +
    ggtitle("Fare Evasion Arrest Intensity vs Poverty by Race", 
      subtitle = "Subway stations in Brooklyn (2016)") +
    # Modify legend title and text
    scale_color_discrete(name = "Predominantly Black Station",
                         labels=c("No", "Yes"),
                         # Reverse Label Order
                         guide = guide_legend(reverse=TRUE)) +
    # Modify legend aesthetics (optional)
    theme(legend.position = "bottom", 
          legend.background = element_rect(color = "black", fill = "grey90", size = .2, linetype = "solid"), 
          legend.direction = "horizontal",
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 8) )
  
  # add quadratic plot
  ggplot(stations_grouped, aes(x = povrt_all_2016, y = arrperswipe, color=as.factor(nblack))) +
    geom_point()  +
    # Add quadratic curve
    geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) + #add regression line
    # Add axis labels and title
    ylab("Arrests relative to ridership") + xlab("Station area poverty rate") +
    ggtitle("Fare Evasion Arrest Intensity vs Poverty by Race", 
            subtitle = "Subway stations in Brooklyn (2016)") +
    # Modify legend title and text
    scale_color_discrete(name = "Predominantly Black Station",
                         labels=c("No", "Yes"),
                         # Reverse Label Order
                         guide = guide_legend(reverse=TRUE)) +
    # Modify legend aesthetics (optional)
    theme(legend.position = "bottom", 
          legend.background = element_rect(color = "black", fill = "grey90", size = .2, linetype = "solid"), 
          legend.direction = "horizontal",
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 8) )

## -----------------------------------------------------------------------------
## 5. interpret your findings with respect to enforcement bias based on race
##    - are there any key limitations to the analysis that limit interpretation?
##    - is there any additional analysis or data you'd like to explore?
## -----------------------------------------------------------------------------
  
  
  
  