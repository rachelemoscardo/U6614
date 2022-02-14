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

wbgender <- read_csv("worldbank-genderstats.csv", na = "..")

#wait this data isn't "tidy"! diff vars for diff variables is... weird
  
#first, let's keep *rows* with information for 3 variables:
  #SE.TER.GRAD.FE.SI.ZS : 
    #Y: Female share of graduates from STEM programmes, tertiary (%)
  #SH.MMR.LEVE :
    #X1: Length of paid maternity leave (calendar days)
  #SP.UWT.TFRT :
    #X2: Unmet need for contraception (% of married women ages 15-49) 

  keepvars <- c("SE.TER.GRAD.FE.SI.ZS", "SH.MMR.LEVE", "SP.UWT.TFRT")
  stem <- wbgender %>% filter(`Series Code` %in% keepvars)
    #note: use backticks to refer to `Series Code` bc of space in col name 

  
##---------------------------
## 2. prepare analysis df
##---------------------------

## A. create 3 separate df's for each variable
##    - stem.fmshstemgrads, stem.dayspaidmatleave, stem.unmetcontr
##    - each df should be a subset of rows for each variable 
  
  stem.fmshstemgrads <- wbgender %>% filter(`Series Code` == "SE.TER.GRAD.FE.SI.ZS")
  stem.dayspaidmatleave <- wbgender %>% filter(`Series Code` == "SH.MMR.LEVE")
  stem.unmetcontr <- wbgender %>% filter(`Series Code` == "SP.UWT.TFRT")
  summary(stem.fmshstemgrads)
    #uh oh, way too many missing values in any given year!
    #obs come from diff surveys in diff years. values of vars may change slowly.
    #let's try using the maximum over all years to reduce NAs
      #i.e. get max fem share of STEM grads for every year (2011-2020)
  
  

## B. create 'analysis variables' = max value across all years for each country
##    - hint: use mutate to create a new column
##    - hint: with mutate, use the pmax() function to get the max across rows
##    - pay attention to NAs
##    - only keep new cols for each analysis var, `Country Name`, `Country Code`
  
  stem.fmshstemgrads <- stem.fmshstemgrads %>% 
    mutate(fmshstemgrads = pmax(`2011 [YR2011]`, `2012 [YR2012]`, `2013 [YR2013]`, 
                                `2014 [YR2014]`, `2015 [YR2015]`, `2016 [YR2016]`,
                                `2017 [YR2017]`, `2018 [YR2018]`, `2019 [YR2019]`, na.rm = TRUE)) %>% 
    select(`Country Name`, `Country Code`, fmshstemgrads) 

  stem.unmetcontr <- stem.unmetcontr %>% 
    mutate(unmetcontr = pmax(`2011 [YR2011]`, `2012 [YR2012]`, `2013 [YR2013]`, 
                                `2014 [YR2014]`, `2015 [YR2015]`, `2016 [YR2016]`,
                                `2017 [YR2017]`, `2018 [YR2018]`, `2019 [YR2019]`, na.rm = TRUE)) %>% 
    select(`Country Name`, `Country Code`, unmetcontr) 
  
  stem.dayspaidmatleave <- stem.dayspaidmatleave %>% 
    mutate(dayspaidmatleave = pmax(`2011 [YR2011]`, `2012 [YR2012]`, `2013 [YR2013]`, 
                                `2014 [YR2014]`, `2015 [YR2015]`, `2016 [YR2016]`,
                                `2017 [YR2017]`, `2018 [YR2018]`, `2019 [YR2019]`, na.rm = TRUE)) %>% 
    select(`Country Name`, `Country Code`, dayspaidmatleave) 

  
  #here's an alternative solution that avoids hardcoding column names
  stem.fmshstemgrads <- stem.fmshstemgrads %>% 
    mutate(fmshstemgrads = do.call(pmax, c(stem.fmshstemgrads[,5:14], na.rm=TRUE)))
  
  
  
## C. join 3 new df's together to get a single "tidy" dataframe
##    - including 3 analysis variables and `Country Name` and `Country Code`

  stem.cross <- inner_join(stem.fmshstemgrads, stem.unmetcontr) %>% 
    inner_join(stem.dayspaidmatleave) %>% 
    na.omit()

  #think about sample selection issues! 
  #are missing observations for 3 analysis variables randomly distributed?

    
  
##---------------------------
## 3. exploratory analysis
##---------------------------


## A. explore relationship between days paid maternity leave & fem share of STEM grads
    ggplot(stem.cross, 
           aes(x = dayspaidmatleave, y = fmshstemgrads, label = `Country Code`, color = unmetcontr)) + 
      geom_point() + 
      stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.2) +
      scale_colour_gradient(low = "blue", high = "red") +
      ylab("Female share of graduates from STEM programs") + xlab("Days of paid maternity leave") +
      labs(color = "Unmet need \nfor contraception \n(% of married women \nages 15-49)") + 
      theme(legend.position = "right")
  
    
  #how would you describe the correlation?
    #what variation are we using? 
    
    cor(stem.cross$dayspaidmatleave, stem.cross$fmshstemgrads)
      #it's is positive (though not linear)
      #we're using cross-sectional variation across countries

    
## B. could other country-level differences in part explain this relationship?
    cor(stem.cross$dayspaidmatleave, stem.cross$unmetcontr)
    
    #there surely are! this is the usual problem w/cross-sectional variation
    #this cross-sectional variation in X (dayspaidmatleave) is endogenous!
    #i.e. it isn't random w/respect to other determinants of Y, such as unmet contraception need (X2)
    

## C. what can we do to improve internal validity?
    #i.e. we at least want to identify a more informative correlation, if not a true causal effect
    #trying to explicitly control for all of these other factors is usually an uphill battle
    #a stronger research design might focus on...
      #time variation "within-countries" (using panel data & fixed effects)
      #even better: exploit sharper policy changes giving variation in X    

  

  