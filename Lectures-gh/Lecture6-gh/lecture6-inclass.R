################################################################################
##
## [ PROJ ] Lecture 6: Using ggplot to plot fare evasion enforcement trends
## [ FILE ] Lecture6-startclass.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Oct 19, 2021 >
##
################################################################################


## --------------------------------------
## 1. load libraries and check directory
## --------------------------------------

library(readxl)
library(tidyverse)

getwd()

## -----------------------------------------
## 2. load and inspect NYPD enforcement data
## -----------------------------------------

df1 <- read_excel("fare evasion - citywide.xlsx", sheet = "Sheet1")
df1.r <- df1 %>% filter(action == "arrests")

ggplot() + 
  geom_bar(data = df1, aes(x = year, y = total * 2000 / 100000, 
                           fill = factor(action, levels = c("summonses", "arrests"))), 
           position = "stack", stat = "identity") +
  geom_line(data = df1.r, aes(x = year, y = ridership), colour = "cyan4", size = 1) + 
  geom_point(data = df1.r, aes(x = year, y = ridership), colour = "cyan4") + 
  scale_y_continuous(
    name = "subway ridership (billions)", 
    sec.axis = sec_axis(~ . * 100000 / 2000 , name = "enforcement actions")) +
  labs(fill = "") + 
  theme_minimal() +
  xlab("") +
  labs(title = "Subway fare evasion enforcement & ridership over time",
       caption = "Data source: MTA Transit and Bus Committee Meeting books") +
  theme(plot.caption = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "PuRd")
