#libraries used:
library(vroom)
library(timetk)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(gridExtra)
library(modeltime) #Extensions of tidymodels to time series
library(forecast)

# Load in Data
train <- vroom("C:/Users/eaa54/Documents/School/STAT348/StoreItemDemand/train.csv")
test <- vroom("C:/Users/eaa54/Documents/School/STAT348/StoreItemDemand/test.csv")

# Make some Exploratory ACF Plots
store1item1 <- train %>%
  filter(store == 1 & item == 1)

a <- store1item1 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

store1item2 <- train %>%
  filter(store == 1 & item == 2)

b <- store1item2%>%
  pull(sales) %>%
  forecast::ggAcf(.)

store1item3 <- train %>%
  filter(store == 1 & item == 3)

c <- store1item3%>%
  pull(sales) %>%
  forecast::ggAcf(.)

store1item4 <- train %>%
  filter(store == 1 & item == 4)

d <- store1item4%>%
  pull(sales) %>%
  forecast::ggAcf(.)

grid.arrange(a,b,c,d)

#########################
##EXPONENTIAL SMOOTHING##
#########################
# 1 store-item combo
train1 <- train %>% 
  filter(store==3, item==17)

cv_split <- time_series_split(train1, assess="3 months", cumulative = TRUE)

cv_split %>%
tk_time_series_cv_plan() #Put into a data frame

# Exp Smoothing Model
es_model <- exp_smoothing() %>%
set_engine("ets") %>%
fit(sales~date, data = training(cv_split))

# Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

# Visualize CV results
pa1 <- cv_results %>%
modeltime_forecast(new_data = testing(cv_split),
                   actual_data = train1) %>%
plot_modeltime_forecast(.interactive=TRUE)

# Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(.interactive = FALSE)

# Refit to all data then forecast
es_fullfit <- cv_results %>%
modeltime_refit(data= train1)

# Format for Kaggle
es_preds <- es_fullfit %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=test, by="date") %>%
select(id, sales)

p1 <- es_fullfit %>%
modeltime_forecast(h = "3 months", actual_data = train1) %>%
plot_modeltime_forecast(.interactive=FALSE)

# another store-item combo
train2 <- train %>% 
  filter(store==1, item==17)

cv_split <- time_series_split(train2, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

# Exp Smoothing Model
es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

# Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

# Visualize CV results
pa2 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

# Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

# Refit to all data then forecast
es_fullfit <- cv_results %>%
  modeltime_refit(data = train2)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

p2 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(pa1, pa2, p1, p2, nrows=2)

##########
##SARIMA##
##########

# 1 store-item combo
train1 <- train %>% 
  filter(store==3, item==17)

cv_split <- time_series_split(train1, assess = "3 months", cumulative = TRUE)

arima_recipe <- recipe(sales ~ date, train1) # model with no feature engineering using store 3, item 17

arima_model <- arima_reg(seasonal_period = 365,
                         non_seasonal_ar = 5, # default max p to tune
                         non_seasonal_ma = 5, # default max q to tune
                         seasonal_ar = 2, # default max P to tune
                         seasonal_ma = 2, #default max Q to tune
                         non_seasonal_differences = 2, # default max d to tune
                         seasonal_differences = 2) %>% #default max D to tune
set_engine("auto_arima")

# Set Workflow
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(cv_split))

# Cross-validate to tune model
cv_arima_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

# Visualize CV results
pa1 <- cv_arima_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = train1) %>%
  plot_modeltime_forecast(.interactive=TRUE)

# Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
modeltime_refit(data = training(cv_split))

# Predict for all the observations in storeItemTest
fullfit %>%
modeltime_forecast(new_data = testing(cv_split),
                   actual_data = training(cv_split)) %>%
plot_modeltime_forecast(.interactive=TRUE)

p1 <- fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

train2 <- train %>% 
  filter(store==4, item==1)

cv_split <- time_series_split(train2, assess = "3 months", cumulative = TRUE)

arima_recipe <- recipe(sales ~ date, train2) # model with no feature engineering using store 3, item 17

arima_model <- arima_reg(seasonal_period = 365,
                         non_seasonal_ar = 5, # default max p to tune
                         non_seasonal_ma = 5, # default max q to tune
                         seasonal_ar = 2, # default max P to tune
                         seasonal_ma = 2, #default max Q to tune
                         non_seasonal_differences = 2, # default max d to tune
                         seasonal_differences = 2) %>% #default max D to tune
  set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(cv_split))

# Cross-validate to tune model
cv_arima_results <- modeltime_calibrate(arima_wf,
                                        new_data = testing(cv_split))

# Visualize CV results
pa2 <- cv_arima_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

# Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
  modeltime_refit(data = training(cv_split))

# Predict for all the observations in storeItemTest
fullfit %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive=TRUE)

p2 <- fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(pa1, pa2, p1, p2, nrows=2)

####################
##FACEBOOK PROPHET##
####################
train_prophet <- train %>% 
  filter(store==3, item==17)

cv_split_prophet <- time_series_split(train1, assess="3 months", cumulative = TRUE)

# Prophet Model
prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split_prophet))

# Cross-validate to tune model
cv_results_prophet <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split_prophet))

# Visualize CV results
proph1 <- cv_results_prophet %>%
  modeltime_forecast(new_data = testing(cv_split_prophet),
                     actual_data = train_prophet) %>%
  plot_modeltime_forecast(.interactive=TRUE)

# Refit to all data then forecast
prophet_fullfit <- cv_results_prophet %>%
  modeltime_refit(data= train_prophet)

# format for Kaggle
prophet_preds <- prophet_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

proph1_bottom <- prophet_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

train2_prophet <- train %>% 
  filter(store==1, item==17)

cv_split_2 <- time_series_split(train2_prophet, assess="3 months", cumulative = TRUE)

# Prophet Model
prophet_model2 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split_2))

# Cross-validate to tune model
cv_results_prophet2 <- modeltime_calibrate(prophet_model2,
                                  new_data = testing(cv_split_2))

# Visualize CV results
proph2 <- cv_results_prophet2 %>%
  modeltime_forecast(new_data = testing(cv_split_2),
                     actual_data = train2_prophet) %>%
  plot_modeltime_forecast(.interactive=TRUE)

# Refit to all data then forecast
prophet_fullfit2 <- cv_results_prophet2 %>%
  modeltime_refit(data = train2_prophet)

proph2_bottom <- prophet_fullfit2 %>%
  modeltime_forecast(h = "3 months", actual_data = train2_prophet) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(proph1, proph2, proph1_bottom, proph2_bottom, nrows=2)
