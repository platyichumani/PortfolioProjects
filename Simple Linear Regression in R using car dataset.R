#read in data set
DF <- read.csv("car data.csv")

#check for any missing values
list_na <- colnames(DF)[apply(DF,2,anyNA)]
list_na

#read in dataset and replace the blanks with NA
DF <- read.csv("car data.csv",na.strings = c("","NA"))

list_na <- colnames(DF)[apply(DF,2,anyNA)]
list_na

# determine which observation has the missing value
which(is.na(DF$Car_Name))
which(is.na(DF$Kms_Driven))
which(is.na(DF$Transmission))

#finding the class of the variables
lapply(DF,class)

#converting variables to categorical variables
DF$Owner <- as.character(DF$Owner)

class(DF$Owner)

# REMOVE THOSE OBSERVATION WITH NA'S
DF <- na.omit(DF)

# FIND THE AGE OF THE CAR
DF$PRESENT_YEAR <- 2023

DF$AGE <- (DF$PRESENT_YEAR - DF$Year)

# LET US PREDICT THE PRICE OF THE CAR USING THE AGE OF THE CAR
# START BY DROPPING ALL THE OTHER VARIABLES FROM THE DATASET
DF_MODEL <- DF[-c(1,2,4,5,6,7,8,9,10)]

#ASSUMPTION 1
# MULTICOLINEARITY

#ASSUMPTION 2
# CHECK WHETHER THE DEPENDENT VARIABLE FOLLOWS A NORMAL DISTRIBUTION
library("ggplot2", "broom", "ggpubr")

hist(DF_MODEL$Selling_Price)

#ASSUMPTION 3
#THE RELATIONSHIP BETWEEN THE INDEPENDENT AND THE DEPENDENT VARIABLES MUST BE
#LINEAR

plot(Selling_Price ~ AGE, data=DF_MODEL)

#CALCULATE Z-SCORE
DF_MODEL$ZSCORE  <- ((DF_MODEL$Selling_Price)-mean(DF_MODEL$Selling_Price))/(sd(DF_MODEL$Selling_Price))
DF_MODEL$ZSCORE2 <- ((DF_MODEL$AGE)-mean(DF_MODEL$AGE))/(sd(DF_MODEL$AGE))

#CREATE DATASET EXCLUDING ZSCORES GREATER THAN 3 AND LESS THAN -3
library("sqldf")
DF_MODEL <- sqldf(" SELECT Selling_Price, AGE
                      FROM DF_MODEL
                     WHERE ZSCORE BETWEEN -3 AND 3
                       AND ZSCORE2 BETWEEN -3 AND 3")

#check scatterplot again
plot(Selling_Price ~ AGE, data=DF_MODEL)

#ASSUMPTION 4
# HOMOSCHEDASTICITY
# WE WILL CHECK THIS AFTER WE MAKE THE MODEL

#SPLIT DATA INTO A TRAINING SET AND A TEST SET
#SET THE SEED
set.seed(1)

#use 70% of the data as training set and the remaining 30% as the test set
sample <- sample(c(TRUE,FALSE),nrow(DF_MODEL), replace=TRUE, prob = c(0.7,0.3))
train  <- DF_MODEL[sample, ]
test   <- DF_MODEL[!sample, ]

#training model
Selling_Price <- lm(Selling_Price ~ AGE, data=train)

summary(Selling_Price)

#check for homoschedasticity
par(mfrow=c(2,2))
plot(Selling_Price)
par(mfrow=c(1,1))

#plot the regression model
SP_GRAPH <- ggplot(DF_MODEL, aes(x=AGE, y=Selling_Price)) + geom_point()
SP_GRAPH + geom_smooth(method="lm",col="black")

# predict
PREDICTION <- predict(Selling_Price, newdata=test)

RMSE <- sqrt(sum((exp(PREDICTION)- test$Selling_Price)^2)/length(test$Selling_Price))
c(RMSE=RMSE, R2=summary(Selling_Price)$r.squared)

par(mfrow=c(1,1))
plot(test$Selling_Price,exp(PREDICTION))
