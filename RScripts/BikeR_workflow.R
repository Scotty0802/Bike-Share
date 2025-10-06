library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(dplyr)
library(patchwork)

train<- vroom("C:/Users/riley/Desktop/Fall 2025/Stat 348/data/bike-sharing-demand/train.csv")
test <- vroom("C:/Users/riley/Desktop/Fall 2025/Stat 348/data/bike-sharing-demand/test.csv")

train <- train |>
  #mutate(workingday = as.factor(workingday),
   #      holiday = as.factor(holiday)) |>
  #mutate(weather = factor(weather), 
  #                         levels = c("1","2","3","4"),
  #                         labels = c("Clear to Cloudy","Very Cloudy","Percipitation","Heavy Percipitation"))) |>
  # mutate(season = factor(season,
  #                        levels = c("1","2","3","4"),
  #                        labels = c("Spring","Summer","Fall","Winter"))) |>
  select(-casual, -registered)

###
my_linear_model <- linear_reg() %>% #Type of model4
  set_engine("lm") %>% # Engine = What R function to use5
  set_mode("regression") %>% # Regression just means quantitative response6
  fit(formula = log1p(count) ~ . -datetime -holiday -workingday, data = train)


## Generate Predictions Using Linear Model9
bike_log_predictions <- predict(my_linear_model,new_data=test) # Use fit to predict11
bike_predictions <- bike_log_predictions |>
  mutate(count = expm1(.pred))## Look at the output


kaggle_submission <- bike_predictions %>%
bind_cols(., test) %>% #Bind predictions with test data3
  select(datetime, .pred) %>% #Just keep datetime and prediction variables4
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)5
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)6
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle7

## Write out the file9
vroom_write(x = kaggle_submission, 
            file = "C:/Users/riley/Desktop/Fall 2025/Stat 348/Bike/kaggle_submission.csv", 
            delim = ",")
