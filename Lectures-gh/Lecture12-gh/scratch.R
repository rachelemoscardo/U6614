rm(list=ls())
library(tidyverse)
library(lfe)
library(fixest)
library(coeftest)
library(lmtest)
library(estimatr)

setwd('~/courses/S22/Backwork/Medicaid')

# Open data
data = read.csv('Data/mvp_mcaid.csv')  %>%
  filter(stfips != 54 & stfips != 4) %>%
  mutate(HC_Texp_1 = ifelse(exp==-4, crate0, 0),
         HC_Texp_5 = ifelse(exp==0, crate0, 0),
         HC_Texp_12 = ifelse (exp==7, crate0, 0),
         HCPOST = ifelse(exp>0 & exp<7, crate0, 0),
         D = ifelse(exp>0 & exp<7, hc, 0))

# For export:
data %>%
  select(stfips, region, year, year_mcaid=ymcaid, income_pc=pcinc, hospitals_pc=hpc, beds_pc = bpc,
         child_pop=ch_pop, afdc_rate=crate0, public_insurance_child=rmvp_ch) %>%
  write.csv('medicaid_coverage.csv', row.names=F)

df2 = read.csv('Data/vs_mcaid.csv') %>% 
  filter(exp > -8 & exp < 9) %>%
  select(stfips, region, year, year_mcaid=ymcaid, white, income_pc=pcinc, hospitals_pc=hpc, beds_pc = bpc,
         mort_rate=amrch, child_pop=popch, afdc_rate=crate0) %>%
  write.csv('medicaid_mortality.csv', row.names=F)

list(coverageDF=df1, mortalityDF=df2) %>% saveRDS('Data/medicaid.rds')

# Define this in code?
hc = data %>%
  filter(exp == 0) %>%
  mutate(hct = ifelse(crate0 > median(crate0), 1, 0))

aggregate = data %>% 
  mutate(rmvp_ch = ifelse(stfips==18 & year == 1969, NaN, rmvp_ch)) %>%
  group_by(exp) %>%
  summarize(rmvp_ch_t = weighted.mean(rmvp_ch, w=ch_pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(hc="All States")

eligSpec = data %>% 
  mutate(rmvp_ch = ifelse(stfips==18 & year == 1969, NaN, rmvp_ch)) %>%
  group_by(exp, hc) %>%
  summarize(rmvp_ch_t = weighted.mean(rmvp_ch, w=ch_pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(hc=ifelse(hc==1, "High-Eligibility States", "Low-Eligibility States")) %>%
  bind_rows(aggregate) %>%
  filter(exp>-4 & exp < 7)

p1 = ggplot(eligSpec, aes(x=exp, y=rmvp_ch_t, group=hc)) +
  geom_line(aes(linetype=hc)) +
  geom_vline(aes(xintercept=-1)) +
  geom_point(aes(shape=hc)) +
  labs(
    x= "Years Since Medicaid Implementation",
    y= "MVP Rate, Children 0-19") +
  scale_linetype_manual(values=c("solid", "dashed", "dashed")) +
  scale_shape_manual(values=c(20, 15, 0)) +
  theme_classic() +
  theme(
    legend.title= element_blank(),
    legend.position=c(0.8,0.2),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="black" ))


### Regressions

## First stage

# Replicate first stage 
# SEs are off for some reason??
mod = feols(rmvp_ch ~ HCPOST + HC_Texp_1 + HC_Texp_5 + HC_Texp_12 + pcinc + hpc + bpc 
            | stfips + region^year + ymcaid^year,
            data=data, weight=data$ch_pop)
summary(mod)

# This fixes it, but I don't trust this random package...
tmod = lm_robust(
  rmvp_ch ~ HCPOST + HC_Texp_1 + HC_Texp_5 + HC_Texp_12 + pcinc + hpc + bpc +
    region:year + ymcaid:year + stfips - 1, 
  data=data, weight=ch_pop, cluster=stfips, se_type="stata")

# Try binary hc option
# Basic w/ only year and state FE
mod = feols(
  rmvp_ch ~ D + pcinc + hpc + bpc 
  | stfips + year,
  data=data, weight=data$ch_pop)
summary(mod, robust=T)

# Might want to control for other time varying shocks affecting all states
# in a region, or common to all regions that receive medicaid in the same year?
# In this case it makes our estimate larger.
mod = feols(
  rmvp_ch ~ D + pcinc + hpc + bpc 
  | stfips + region^year + ymcaid^year,
  data=data, weight=data$ch_pop)
summary(mod)

filteredData = data 
mod = feols(
  rmvp_ch ~ i(factor(exp), hc, ref=-1) + pcinc + hpc + bpc 
  | stfips + region^year + ymcaid^year,
  data=filteredData, 
  weight=filteredData$ch_pop)
summary(mod)

iplot(
  mod,
  pt.join = TRUE,
  ci.join = TRUE,
  ci.lwd=0,
  grid.par=list(vert=FALSE),
  drop=c("-4", "7"),
  main="Medicaid and Children's Public Insurance Use",
  ylab="Share using public insurance (x100)",
  xlab="Years since Medicaid implementation")


## Reduced form

data_w = read.csv('Data/vs_mcaid.csv') %>% 
  filter(exp > -8 & exp < 9) %>%
  mutate(D = ifelse(exp>0, hc, 0))

mod = feols(
  lnamrch ~ D + pcinc + hpc + bpc 
  | stfips + region^year + ymcaid^year,
  data=data_w, weight=data_w$popch)
summary(mod)


# Replicate White
data_w1 = read.csv('Data/vs_mcaid.csv') %>% 
  filter(white==1, exp > -8 & exp < 9) %>%
  mutate(D = ifelse(exp>0, hc, 0))

mod = feols(lnamrch ~ HCPOST + HC_Texp_1 + HC_Texp_18 + HC_Texp_28 + pcinc + hpc + bpc 
            | stfips + region^year + ymcaid^year,
            data=data_w1, weight=data_w1$popch)
summary(mod)

# Binary treatment White
mod = feols(
  lnamrch ~ D + pcinc + hpc + bpc 
  | stfips + region^year + ymcaid^year,
  data=data_w1, weight=data_w1$popch)
summary(mod)

# ES White
mod = feols(
  lnamrch ~ i(factor(exp), hc, ref=-1) + pcinc + hpc + bpc 
  | stfips + region^year + ymcaid^year,
  data=data_w1, 
  weight=data_w1$popch)
summary(mod)

p_w1 = iplot(
  mod,
  pt.join = TRUE,
  ci.join = TRUE,
  ci.lwd=0,
  grid.par=list(vert=FALSE),
  main="Medicaid and Children's Public Insurance Use",
  ylab="Share using public insurance (x100)",
  xlab="Years since Medicaid implementation")



# Replicate Nonwhite
data_w0 = read.csv('Data/vs_mcaid.csv') %>% 
  filter(white==0, exp > -8 & exp < 9) %>%
  mutate(D = ifelse(exp>0, hc, 0))

mod = feols(lnamrch ~ HCPOST + HC_Texp_1 + HC_Texp_18 + HC_Texp_28 + pcinc + hpc + bpc 
            | stfips + region^year + ymcaid^year,
            data=data_w0, weight=data_w0$popch)
summary(mod)

# Binary treatment Nonwhite
mod = feols(
  lnamrch ~ D + pcinc + hpc + bpc 
  | stfips + region^year + ymcaid^year,
  data=data_w0, weight=data_w0$popch)
summary(mod)

data_test = data_w0 %>%
  filter(year > ymcaid) %>%
  group_by(stfips) %>%
  summarize_all(mean) %>%
  ungroup()

# CS test?
mod = lm(
  lnamrch ~ D + pcinc + hpc + bpc,
  data=data_test, weight=data_test$popch)
summary(mod)

test = data_w0 %>%
  filter(year == ymcaid) %>%
  mutate(treat = ifelse(crate0 > median(crate0), 1, 0)) %>%
  select(stfips, treat) %>%
  right_join(data_w0, by='stfips') %>%
  mutate(median_cr = crate0)

any(test$hc == test$treat)  

# ES Nonwhite
mod = feols(
  lnamrch ~ i(factor(exp), hc, ref=-1) + pcinc + hpc + bpc 
  | stfips + region^year + ymcaid^year,
  data=data_w0, 
  weight=data_w0$popch)
summary(mod)

p_w2 = iplot(
  mod,
  pt.join = TRUE,
  ci.join = TRUE,
  ci.lwd=0,
  grid.par=list(vert=FALSE),
  main="Medicaid and Children's Public Insurance Use",
  ylab="Share using public insurance (x100)",
  xlab="Years since Medicaid implementation")


p_w1
plot(p_w2)



#p = iplot(mod, pt.join = TRUE)
#p + theme_minimal()
# Syntax reminder... 
# felm(y ~ x2 | x3:id1 + id1, df)


