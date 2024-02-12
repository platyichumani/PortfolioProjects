install.packages("fpp3")
library("fpp3")

#install.package
install.packages("readxl")
library("readxl")

#Read data

data <- read_excel("EXCEL - CPI(5 and 8 digit) from Jan 2017 (202309).xlsx")

names(data)

data <- data[ , ! names(data) %in% c("H01", "H02", "H03", "H05", "H06", "H07", "Weight (All urban)")]


#Transpose
data2 <- t(data)

#Convert data2 to csv file
write.csv(data2, file = "data2.csv")

#Reading dataset
DF <- read.csv("data2.csv")

#Remove H04
DF <- DF[-1,]

names(DF)

DF <- DF[ ,  names(DF) %in% c("X", "Rice", "Loaf.of.white.bread", "Loaf.of.brown.bread",
                              "Spaghetti", "Macaroni")]

colnames(DF)[colnames(DF) == "X"]                   <- "Month"
colnames(DF)[colnames(DF) == "Rice"]                <- "CPI_Rice"
colnames(DF)[colnames(DF) == "Loaf.of.white.bread"] <- "CPI_Wbread"
colnames(DF)[colnames(DF) == "Loaf.of.brown.bread"] <- "CPI_Bbread"
colnames(DF)[colnames(DF) == "Spaghetti"]           <- "CPI_Spaghetti"
colnames(DF)[colnames(DF) == "Macaroni"]            <- "CPI_Macaroni"

install.packages("forecast")
library("forecast")

DF2 <- DF %>%
  mutate(Month = as.Date(x = paste0("01/", substr(Month, 6, 7), "/", substr(Month, 2, 5)), format = "%d/%m/%Y") %>% 
           yearmonth) %>%
  select(-c(CPI_Spaghetti, CPI_Macaroni, CPI_Rice, CPI_Wbread)) %>%
  as_tsibble(index = Month)

DF2 <- DF2[ , ! names(DF2) %in% c("CPI_Bbread")]
DF2 %>% distinct(CPI_Bbread)

CPI <- c(82.8, 82.5, 82, 82.3, 82.7, 82.9, 83, 82.1, 81.2, 81.60, 
         80.7, 81.3, 81.7, 82.6, 81.80, 81.90, 81.8, 81.3, 82.1, 80.7, 83, 
         83.4, 83.6, 84.1, 84.5, 87.1, 89.7, 90.1, 90.3, 91, 91, 90.5,
         89.3, 89.7, 90.2, 89.7, 89.2, 89.2, 89.8, 90.1, 92.6, 92.7, 94.5,
         96.6, 96.4, 96.6, 97, 97.9, 97.8, 97.4, 97.4, 96.5, 96.5, 96.9,
         96.3, 96.5, 96.9, 96.9, 98.6, 100, 100, 100.3, 102.8, 104, 104.3,
         107.8, 111.2, 113.3, 118.4, 118.6, 118.7, 118.7, 120.2, 120.6, 
         122.1, 123.1, 123.2, 123.6, 123, 122.3, 123)

CPI_Brown_bread <- data.frame(CPI)

ID <- c(1:81)
ID <- data.frame(ID)
DF3 <- cbind(CPI_Brown_bread, DF2)

DF4 <- DF3 %>%
  as_tsibble(index = Month)
##
autoplot(DF4, CPI) +
  labs(title = "Time Plot: CPI for a loaf of brown bread")

DF4 %>%
  gg_season(CPI, labels = "both") +
  labs(title = "Consumer Price Index for a loaf of brown bread(seasonal)")

DF4 %>%
  ggplot(aes(x = Month, y = CPI)) + 
  geom_point() + 
  labs(title = "Consumer Price Index for a loaf of brown bread")

DF4 %>% ACF(CPI) %>% autoplot() + 
  labs(title = "ACF of Consumer Price Index for a loaf of brown bread")      
##########

install.packages("seasonal")
library("seasonal")

x11_dcmp <- DF4 %>%
  model(x11 = X_13ARIMA_SEATS(CPI ~ x11())) %>%
  components()

autoplot(x11_dcmp) + labs(title = "Decomposition of Consumer Price Index using X-11.")


##2
fit <- DF4 %>% 
  filter(!is.na(CPI)) %>% 
  model(SNAIVE(CPI))

fit %>% gg_tsresiduals() + labs(title = "Consumer Price Index for a loaf of brown bread(SNAIVE)")

##3
fit %>% forecast(h = 4) %>% 
  autoplot(DF4)

#Fit a regression line to the trend component
fit2 <- DF4  %>% 
  model(TSLM(CPI ~ trend()))
report(fit2)

# Use the tidy() function to view the results of the model
tidy(fit2)

#Predict the next value of your time series using your model fit2
FC2 <- fit2 %>% forecast(h = 1)

FC2 %>% 
  autoplot(DF4)
install.packages("seasonalview")
library("seasonalview")

view(FC2)

#Use the ETS() function to estimate the equivalent model for simple exponential
#smoothing

fit3 <- DF4 %>% 
  model(ses = ETS(CPI ~ error("A") + trend("A") + season("A")))
# since this time series was heavily seasonal and trended

# Find the optimal values of α and ℓ0
report(fit3)

# you get these values
alpha = 0.9998192 
beta  = 0.1339645 
gamma = 0.0001219236 

#Generate forecasts
FC3 <- fit3 %>% 
  forecast(h = 4)

FC3 %>% 
  autoplot(DF4) + labs(title = "Forecast of CPI for a loaf of brown bread using ETS model")


##########


#Use ARIMA() to find an appropriate ARIMA model of your time series

library(urca)

fit4 <- DF4 %>% 
  model(arima = ARIMA(CPI))

report(fit4)
Model: ARIMA(1,1,1) w/ drift

FC4 <- fit4 %>% 
  forecast(h = 4)

FC4 %>%
  autoplot(DF4) + labs(title = "Forecast of Consumer Price Index for a loaf of brown bread using ARIMA model")

