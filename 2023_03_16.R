# SEP
# 2023-03-16
# Stock Collapse

library(tidyverse)
load('data/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData') # reads in an r file
head(area)

glimpse(stock)
glimpse(timeseries_values_views)
glimpse(taxonomy)

fish = timeseries_values_views %>%
  left_join(stock) %>%
  left_join(taxonomy) %>%
  select(stockid, stocklong, year, TCbest, tsn, 
         scientificname, commonname, region, FisheryType, taxGroup)

glimpse(tsmetrics)
tsmetrics %>%
  filter(tsshort == "TCbest")


glimpse(fish)
dim(timeseries_values_views)
dim(fish)

ggplot() + 
  geom_line(aes(x=year, y=TCbest, color= stockid), data = fish) +
  theme(legend.position = "none")

fish %>%
  filter(TCbest > 20000000) # red line is an acadian redfish 

fish = fish %>%
  filter(stockid != 'ACADREDGOMGB')

ggplot() + 
  geom_line(aes(x=year, y=TCbest, color= stockid), data = fish) +
  theme(legend.position = "none")

fish %>%
  filter(TCbest > 6000000)

fish %>%
  filter(scientificname == "Gadus morhua") %>%
  distinct(region)

cod_can = fish %>%
  filter(scientificname == "Gadus morhua", 
  region == "Canada East Coast", 
  !is.na(TCbest))
head(cod_can)

ggplot(data = cod_can) + 
  geom_line(aes(x=year, y=TCbest, color = stockid))

cod_can_total = cod_can %>%
  group_by(year) %>%
  summarize(total_catch = sum(TCbest))
head(cod_can_total)
ggplot(data = cod_can_total) + 
  geom_line(aes(x=year, y=total_catch))

dat = c(1,3,6,2,3,9,-1)
dat_max = cummax(dat)
dat_sum = cumsum(dat)
cbind(dat, dat_max, dat_sum)


cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch), 
         collapse = total_catch<= 0.1*historical_max_catch)

head(cod_collapse)
tail(cod_collapse)

cod_collapse_year = cod_collapse %>%
  filter(collapse == TRUE) %>%
  summarize(year = min(year)) %>%
  pull(year) # gets rid of tibble table

cod_collapse_year 

ggplot() + 
  geom_line(aes(x=year, y=total_catch, color = collapse), data = cod_collapse) + 
  geom_vline(xintercept = cod_collapse_year)

# Global stock collapse
collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest), 
         current_collapse = TCbest <0.1*historical_max_catch, 
         collapsed_yet = cumsum(current_collapse)> 0) %>%
  ungroup()
head(collapse)  
glimpse(collapse)


collapse_yr = collapse %>%
  group_by(stockid, stocklong, region) %>% # really just groups by stockid, but retains region
  filter(collapsed_yet == TRUE) %>% 
  summarize(first_collapse_yr = min(year)) %>%
  ungroup()
glimpse(collapse_yr)

ggplot(data = collapse_yr, aes(x=first_collapse_yr)) +
  geom_histogram(color="black", fill="white", binwidth=5)





