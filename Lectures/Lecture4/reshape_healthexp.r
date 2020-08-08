
# import wide form data
health <- read.csv("health_spending_per_capita_02-17.csv", stringsAsFactors = FALSE)

# reshape to long form
health_long_temp <- gather(data = health, key = year, value = healthexp, X2002:X2017, factor_key = FALSE)

# clean up year column
health_long <- health_long_temp %>% 
  mutate(year = as.numeric(str_replace_all(year, "X", ""))) %>% 
  rename(country = ï..Country.Name) %>% 
  select(-Country.Code)


str(health_long)

write_csv(health_long, "healthexp_long.csv") 
saveRDS(health_long, file = "healthexp_long.rds")