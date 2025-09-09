library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(dplyr)

train <- vroom("C:/Users/riley/Desktop/Fall 2025/Stat 348/data/bike-sharing-demand/train.csv")
test <- vroom("C:/Users/riley/Desktop/Fall 2025/Stat 348/data/bike-sharing-demand/test.csv")
View(train)
View(test)


train <- train |>
  mutate(weather = as.factor(weather),
         season = as.factor(season),
         workingday = as.factor(workingday),
         holiday = as.factor(holiday))

colSums(is.na(train))
colSums(is.na(test))

#Train Plot
ggplot() +
  geom_histogram(data = train, aes(x = temp, fill = "temp"), 
                 binwidth = 1, alpha = 0.8, color = "black") +
  geom_histogram(data = train, aes(x = atemp, fill = "atemp"), 
                 binwidth = 1, alpha = 0.3, color = "black") +
  scale_fill_manual(values = c("temp" = "grey", "atemp" = "red")) +
  labs(title = "Overlay of Temerature and Air Temperature Histograms",
       x = "Temperature",
       y = "Frequency",
       fill = "Variable") +
  theme_minimal()

ggplot(train, aes(x = weather, fill = weather)) +
  geom_bar(alpha = 0.8, color = "black")

ggplot(train, aes(y = humidity)) + 
  geom_boxplot(fill = "lightgreen")



#Test Plot
ggplot(test, aes(x = temperature)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black")

ggplot(test, aes(y = humidity)) + 
  geom_boxplot(fill = "lightgreen")