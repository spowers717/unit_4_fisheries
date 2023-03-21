# SEP
# 2023-03-21
# glm

source("build_collapse_table.R")
head(fish)
head(collapse)

# logistic regression
model_data = collapse %>%
  group_by(stockid) %>%
  summarize(ever_collapsed = any(current_collapse)) %>%
  ungroup() %>%
  left_join(stock) %>%
  left_join(metadata) %>%
  mutate(FisheryType = as.factor(FisheryType)) %>%
  filter(!is.na(FisheryType))

summary(model_data) # characters, model requires fishery type to be a factor 

# build logistic regression
model_l = glm(ever_collapsed~FisheryType, data = model_data, family ="binomial")
summary(model_l)

model_data %>% distinct(FisheryType) %>% arrange(FisheryType)

FisheryType = model_data %>%
  distinct(FisheryType) 

model_l_predict = predict(model_l, newdata = FisheryType, se.fit = TRUE, type = "response")
# response gives us the prediction as a percentage

head(model_l_predict)
collapse_fishery_type_predictions = cbind(FisheryType, model_l_predict)

ggplot(data = collapse_fishery_type_predictions, aes(x= FisheryType, y=fit, fill = FisheryType)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width = 0.2) +
  coord_flip() 
# default equal to the count, instead say plot the fit for the length of the bar

# Poisson Model

# How many years does a stock spend in a collapsed state?

# UdivUmspref U/Umsy > 1 = overfished true harvest rate divided by the true harvest rate it should be
# B/Bmsy < 1 = low stock (biomass)

u_summary = timeseries_values_views %>%
  filter(!is.na(UdivUmsypref), 
         !is.na(BdivBmsypref)) %>%
  group_by(stockid) %>%
  summarize(yrs_data = n(), 
            ratio_yrs_overfished = sum(UdivUmsypref > 1)/yrs_data, 
            ratio_yrs_low_stock = sum(BdivBmsypref < 1)/yrs_data) %>%
  select(-yrs_data)
head(u_summary)

collapse_summary = collapse %>%
  group_by(stockid) %>%
  summarize(yrs_data = n(), 
            yrs_collapsed = sum(current_collapse)) %>%
  inner_join(u_summary, by = "stockid")
# how many rows is current collapse true
head(collapse_summary)
hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)
# zero inflated, something else explaining the zeros rather than the other numbers
# get rid of the zeroes (stocks that have never collapsed)
# only have the model explain those that have collapsed
collapse_summary_zero_trunc = collapse_summary%>%
  filter(yrs_collapsed>0)
table(collapse_summary_zero_trunc$yrs_collapsed)
head(collapse_summary_zero_trunc)

# Build Model

model_p = glm(yrs_collapsed ~ratio_yrs_overfished + ratio_yrs_low_stock,
              offset(log(yrs_data)), 
              data = collapse_summary_zero_trunc, 
              family = "poisson")
summary(model_p)
# problem with poisson models is that they are over dispersed, high amount of variation in the number of years which could make our model overdispered
library(AER)
AER :: dispersiontest(model_p)
# our model is overdispersed because p -value small
# quick fix to build a quasson model
model_qp = glm(yrs_collapsed ~ratio_yrs_overfished + ratio_yrs_low_stock,
              offset(log(yrs_data)), 
              data = collapse_summary_zero_trunc, 
              family = "quasipoisson")
summary(model_qp)
