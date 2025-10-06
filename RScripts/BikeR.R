library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(dplyr)
library(patchwork)

train<- vroom("C:/Users/riley/Desktop/Fall 2025/Stat 348/data/bike-sharing-demand/train.csv")
test <- vroom("C:/Users/riley/Desktop/Fall 2025/Stat 348/data/bike-sharing-demand/test.csv")
View(train)
View(test)


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


#Train Plot
##
GSeas <- ggplot(train, aes(x = factor(season), y = count, fill = factor(weather))) +
  geom_bar(stat = "identity", alpha = 0.85) +
  theme_minimal() +
  labs(
    x = "Season",
    y = "Total Rentals",
    fill = "Weather Condition",
    title = "Total Rentals by Season and Weather"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

GWeather <- ggplot(train, aes(x = factor(weather), y = count, fill = factor(weather))) +
  geom_bar(stat = "identity", alpha = 0.85) +
  theme_minimal() +
  labs(
    x = "Season",
    y = "Total Rentals",
    fill = "Weather Condition",
    title = "Total Rentals by Season and Weather"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


GDate <- ggplot(data = train, aes(x = datetime, y = count)) + 
  geom_smooth(se = FALSE) +
  theme_minimal()+
  labs(x = "Date", y = "Count of Rentals", title = "Rentals by Date")

Gtemp <- ggplot(data = train) +
  geom_smooth(aes(x = datetime, y = temp, color = "Actual Temperature"), se = FALSE) +
  geom_smooth(aes(x = datetime, y = atemp, color = "Feels Like Temperature"), se = FALSE) +
  scale_color_manual(
    name = "Type of Temperature",
    values = c("Actual Temperature" = "red", "Feels Like Temperature" = "darkblue")
  ) +
  theme_minimal() +
  labs(x = "Date", y = "Degrees in Celsius", title = "Temperature by Date") +
  theme(legend.title = element_text(face = "bold"))

(Gtemp + GWeather)/(GDate + GSeas)


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
