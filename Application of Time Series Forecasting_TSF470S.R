# A

library(fpp3)

aus_acom <- read.csv("aus_accommodation.csv") 

aus_TS <- aus_acom %>%
  mutate(Quarter = paste(Year, Quarter, sep = " ")) %>%
             mutate(Quarter = yearquarter(Quarter)) %>% 
           as_tsibble(index = Quarter, key = State) %>% 
                        filter(State == "Victoria") %>%
              select(-c(Year, Takings, CPI, State))
aus_TS 

# B

? aus_accommodation

# aus_accommodation is a quarterly 'tsibble' containing data on Australian tourist 
# accommodation from short-term non-residential accommodation with 15 or more rooms, 
# 1998 Q1 - 2016 Q2. Occupancy is a percentage of rooms occupied.

# C

autoplot(aus_TS, Occupancy) +
  labs(title = "Time Plot: Occupancy Rates in Victoria",
       x = "Quarter",
       y = "Occupancy Rate (%)")

# The time plot shows there's an increasing trend in occupancy rates in Victoria over the years.
# There is also a clear seasonality pattern with regular ups and downs within each year. 
# This suggests a yearly cycle in the data, possibly related to seasonal factors.

gg_subseries(aus_TS, Occupancy) +
  labs(title = "Subseries Plot: Occupancy Rates in Victoria",
       x = "Quarter",
       y = "Occupancy Rate (%)")

# The subseries plot shows the quarterly pattern of occupancy rates within each year.
# Each quarter shows an increasing trend 
# Q1 and Q4 have the highest average occupancy rates while Q2 and Q3 have the lowest average occupancy rates

aus_TS %>%
  gg_season(Occupancy, labels = "both") +
  labs(y = "Occupancy Rate (%)",
       x = "Quarter",
       title = "Occupancy Rates in Victoria")
# Each year follows roughly the same seasonal pattern
# 2003 and 2004 had a large increase in Q3 compared to the other years

aus_TS %>%
  ggplot(aes(x = Quarter, y = Occupancy)) + 
  geom_point() + 
  labs(x = "Quarter",
       y = "Occupancy Rate (%)",
       title = "Occupancy Rates in Victoria")

# There is a clear positive correlation between quarter and occupancy rate

aus_TS %>% ACF(Occupancy) %>% autoplot()

# D

lambda <- aus_TS %>%
  features(Occupancy, features = guerrero) %>%
  pull(lambda_guerrero)

aus_TS %>%
  autoplot(box_cox(Occupancy, lambda)) + 
  labs(y = "Occupancy", title = latex2exp::TeX(paste0("Transformed Occupancy numbers with $\\lambda$ = ",round(lambda,2))))
# the transformation was not necessary

# E
library(seasonal)

x11_dcmp <- aus_TS %>%
  model(x11 = X_13ARIMA_SEATS(Occupancy ~ x11())) %>%
  components()

autoplot(x11_dcmp) + labs(title = "Decomposition of Occupancy using X-11.")

# F
# The seasonal component shows variation in the X-11 method as the seasonality varies overtime but not by much.

# G
# the seasonal naive bench mark model since the time series is highly seasonal

# H
fit <- aus_TS %>% 
  filter(!is.na(Occupancy)) %>% 
  model(SNAIVE(Occupancy))

# I
fit %>% gg_tsresiduals()
# 3/18 (16.7%) spikes are outside of the blue lines, 
# this suggests we are not dealing with white noise.

# J
fit %>% forecast(h = 4) %>% 
  autoplot(aus_TS)

# K
fit2 <- aus_TS  %>% 
  model(TSLM(Occupancy ~ trend()))

# L
tidy(fit2)
# The occupancy rate has been increasing by an average of 0.175 each year.

# M
FC2 <- fit2 %>% forecast(h = 1)

# N
view(FC2)
# that the mean column of the fable contains the forecasted occupancy rate

# O
fit3 <- aus_TS %>% 
  model(ses = ETS(Occupancy ~ error("A") + trend("A") + season("A")))
# since this time series was heavily seasonal and trended

# P
report(fit3)

# Q 
FC3 <- fit3 %>% 
  forecast(h = 4)

FC3 %>% 
  autoplot(aus_TS)

# R
library(urca)

fit4 <- aus_TS %>% 
  model(arima = ARIMA(Occupancy))

# S
report(fit4)

# Model: ARIMA(1,0,1)(0,1,2)[4] w/ drift 

# T
FC4 <- fit4 %>% 
  forecast(h = 4)

FC4 %>%
  autoplot(aus_TS)

# U
# Arima performed the best since it has the lowest AIC and it looks like the forecast 
# captures the trend and seasonal components nicely.
