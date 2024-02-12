#1
DF <- read.csv("California Housing Data.csv",na.strings = c("","NA"))

#2
list_na <- colnames(DF)[apply(DF,2,anyNA)]
list_na 

#3
DF <- na.omit(DF)

#4
lapply(DF,class)

#5
#everything looks fine

#6
DF <- sqldf(" SELECT DF.*
                FROM DF
               WHERE housing_median_age BETWEEN 30 AND 50
            ORDER BY population")

#7
plot(median_house_value ~ longitude, data=DF)
plot(median_house_value ~ latitude, data=DF)
plot(median_house_value ~ housing_median_age, data=DF)
plot(median_house_value ~ total_rooms, data=DF)
plot(median_house_value ~ total_bedrooms, data=DF)
plot(median_house_value ~ population, data=DF)
plot(median_house_value ~ households, data=DF)
plot(median_house_value ~ median_income, data=DF)

#8
DF$ZSCORE  <- ((DF$median_house_value)-mean(DF$median_house_value))/(sd(DF$median_house_value))

#9
library("sqldf")

DF_2 <- sqldf(" SELECT DF.*
                  FROM DF
                 WHERE ZSCORE BETWEEN -3 AND 3")

#10
set.seed(1)

#11
sample <- sample(c(TRUE,FALSE),nrow(DF_2), replace=TRUE, prob = c(0.8,0.2))
train  <- DF_2[sample, ]
test   <- DF_2[!sample, ]

#12
MODEL_1 <- lm(median_house_value ~ longitude, data=train)
MODEL_2 <- lm(median_house_value ~ latitude, data=train)
MODEL_3 <- lm(median_house_value ~ housing_median_age, data=train)
MODEL_4 <- lm(median_house_value ~ total_rooms, data=train)
MODEL_5 <- lm(median_house_value ~ total_bedrooms, data=train)
MODEL_6 <- lm(median_house_value ~ population, data=train)
MODEL_7 <- lm(median_house_value ~ households, data=train)
MODEL_8 <- lm(median_house_value ~ median_income, data=train)

#13
summary(MODEL_1) #SIGNIFICANT 
summary(MODEL_2) #SIGNIFICANT 
summary(MODEL_3) #NOT SIGNIFICANT 
summary(MODEL_4) #SIGNIFICANT 
summary(MODEL_5) #SIGNIFICANT 
summary(MODEL_6) #SIGNIFICANT 
summary(MODEL_7) #SIGNIFICANT 
summary(MODEL_8) #SIGNIFICANT 

#14
PRED_1 <- predict(MODEL_1, newdata=test)
PRED_2 <- predict(MODEL_2, newdata=test)
PRED_3 <- predict(MODEL_3, newdata=test)
PRED_4 <- predict(MODEL_4, newdata=test)
PRED_5 <- predict(MODEL_5, newdata=test)
PRED_6 <- predict(MODEL_6, newdata=test)
PRED_7 <- predict(MODEL_7, newdata=test)
PRED_8 <- predict(MODEL_8, newdata=test)

#15
PRED_1 <- as.data.frame(PRED_1)
PRED_2 <- as.data.frame(PRED_2)
PRED_3 <- as.data.frame(PRED_3)
PRED_4 <- as.data.frame(PRED_4)
PRED_5 <- as.data.frame(PRED_5)
PRED_6 <- as.data.frame(PRED_6)
PRED_7 <- as.data.frame(PRED_7)
PRED_8 <- as.data.frame(PRED_8)

library(Metrics)
rmse(test$median_house_value, PRED_1$PRED_1)
rmse(test$median_house_value, PRED_2$PRED_2)
rmse(test$median_house_value, PRED_3$PRED_3)
rmse(test$median_house_value, PRED_4$PRED_4)
rmse(test$median_house_value, PRED_5$PRED_5)
rmse(test$median_house_value, PRED_6$PRED_6)
rmse(test$median_house_value, PRED_7$PRED_7)
rmse(test$median_house_value, PRED_8$PRED_8)

#MODEL 1
# RMSE = 118165.3
# R-Square = 0.00128
# An r-squared of 0.1% reveals that 0.1% of the variability observed in median_house_value
# is explained by longitude.

#MODEL 2
# RMSE = 116573.4
# R-Square = 0.01519
# An r-squared of 1.5% reveals that 1.5% of the variability observed in median_house_value
# is explained by latitude.

#MODEL 3
# RMSE = 118095.6
# R-Square = 0.0001195
# An r-squared of 0.01% reveals that 0.01% of the variability observed in median_house_value
# is explained by housing_median_age.

#MODEL 4
# RMSE = 113974.4
# R-Square = 0.07131
# An r-squared of 7.1% reveals that 7.1% of the variability observed in median_house_value
# is explained by total_rooms.

#MODEL 5
# RMSE = 117389.8
# R-Square = 0.0135
# An r-squared of 1.3% reveals that 1.3% of the variability observed in median_house_value
# is explained by total_bedrooms.

#MODEL 6
# RMSE = 117673
# R-Square = 0.005071
# An r-squared of 0.51% reveals that 0.51% of the variability observed in median_house_value
# is explained by population.

#MODEL 7
# RMSE = 117284.7
# R-Square = 0.01661
# An r-squared of 1.7% reveals that 1.7% of the variability observed in median_house_value
# is explained by households.

#MODEL 8
# RMSE = 82586.93
# R-Square = 0.5231
# An r-squared of 52.3% reveals that 52.3% of the variability observed in median_house_value
# is explained by median_income.
# This model also has the lowest RMSE meaning that it performed the best at predicting median_house_value

#16
# Model 8 has the lowest RMSE meaning that median_income performed the best at predicting median_house_value
# median_income explains most of the variation in observed in median_house_value compared to the other 
# independent variables





