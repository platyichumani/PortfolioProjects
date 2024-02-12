#READ IN DATA
DF <- read.csv("protein.csv")

#VARIABLE NAMES
names(DF)

#USING SQL TO SEE WHICH OBSERVATIONS HAVE MISSING VALUES
library(sqldf)

DF_1 <- sqldf("SELECT DF.*
                 FROM DF
                WHERE RedMeat IS NULL")

#USING SQL TO REMOVE DUPLICATE OBSERVATIONS
DF_2 <- sqldf("SELECT DISTINCT DF.*
                 FROM DF")

#USING SQL TO COUNT A CERTAIN CONDITION
DF_3 <- sqldf("SELECT COUNT(Country)
                 FROM DF_2
                WHERE Country='France'")

#USING SQL TO FIND THE AVERAGE OF A VARIABLE
DF_4 <- sqldf("SELECT AVG(Milk)
                 FROM DF_2")

#USING SQL TO FIND THE SUM OF A VARIABLE
DF_5 <- sqldf("SELECT SUM(Fish)
                 FROM DF_2")

#READ IN DATASET
CAR <- read.csv("car data.csv")

#REMOVE DUPLICATE OBSERVATIONS USING SQL
CAR_1 <- sqldf("SELECT DISTINCT CAR.*
                  FROM CAR")

#REMOVE ROWS WITH MISSING VALUES
CAR_2 <- na.omit(CAR_1)

#REMOVE ROWS WITH BLANKS
CAR_3 <- CAR_2[!CAR_2$Car_Name=="",]
CAR_4 <- CAR_3[!CAR_3$Transmission=="",]

#TURNING CATEGORICAL VARIABLES INTO DUMMY VARIABLES SINCE REGRESSION DOES NOT WORK WITH CATEGORICAL VARIABLES
#OPTION 1
CAR_4$Petrol <- ifelse(CAR_4$Fuel_Type=="Petrol",1,0)
CAR_4$Diesel <- ifelse(CAR_4$Fuel_Type=="Diesel",1,0)
CAR_4$CNG    <- ifelse(CAR_4$Fuel_Type=="CNG",1,0)

#OPTION 2
library(fastDummies)

CAR_4 <- dummy_cols(CAR_4,select_columns = "Fuel_Type")

#ASSUMPTION 1 - LINEARITY BETWEEN DEPENDENT AND INDEPENDENT VARIABLES

#Dummy variables meet the assumption of linearity by definition, because they 
#create two data points, and two points define a straight line. 
#There is no such thing as a non-linear relationship for a single variable 
#with only two values.

#PLOT OUR DATA
#OPTION 1
plot(CAR_4$Present_Price,CAR_4$Selling_Price) #LINEAR
plot(CAR_4$Kms_Driven,CAR_4$Selling_Price)    #NOT LINEAR
plot(CAR_4$Petrol,CAR_4$Selling_Price)        #LINEAR
plot(CAR_4$Diesel,CAR_4$Selling_Price)        #LINEAR
plot(CAR_4$CNG,CAR_4$Selling_Price)           #LINEAR

#Dropping a predictor just because it doesn't show a linear relationship with the 
#response when considered alone is usually a bad idea, because that predictor may 
#be useful when used with other predictors.

#ONLY KEEP THE VARIABLES WE NEED FOR OUR MODEL
CAR_4 <- CAR_4[c(3,4,5,10,11,12)]

#OPTION 2
plot(CAR_4)

#ASSUMPTION 2 - MULTICOLLINEARITY BETWEEN INDEPENDENT VARIABLES
#CORRELATION GRAPHS
plot(CAR_4)

#CORRELATION VALUES
cor(CAR_4)

#SPLIT DATA INTO A TRAINING SET AND A TEST SET
#SET THE SEED
set.seed(1)

#use 70% of the data as training set and the remaining 30% as the test set
sample <- sample(c(TRUE,FALSE),nrow(CAR_4), replace = TRUE, prob = c(0.7,0.3))
train  <- CAR_4[sample,]
test   <- CAR_4[!sample,]

#PREDICTION MODEL
names(CAR_4)

Selling_Price <- lm(Selling_Price ~ Present_Price + Kms_Driven + Petrol + Diesel + CNG, data=train)

summary(Selling_Price)
#INTERPRETING THE SUMMARY TABLE

#STEP 1
#It can be seen that p-value of the F-statistic is < 2.2e-16 (<0.05)
#which is highly significant. This means that, at least, one of the predictor 
#variables is significantly related to the outcome variable.

#STEP 2
#To see which predictor variables are significant, you can examine the 
#coefficients table, which shows the estimate of regression beta coefficients 
#and the associated t-statitic p-values:

#For a given the predictor, the t-statistic evaluates whether or not there is 
#significant association between the predictor and the outcome variable, that is
#whether the beta coefficient of the predictor is significantly different from zero.

#It can be seen that, changes in the present price and Kilometers driven are significantly associated to
#changes in Selling price while changes in Diesel,Petrol and CNG are not significantly 
#associated with selling price.

#As the Diesel, Petrol and CNG variables are not significant, remove 
#them from the model

#MODEL ACCURACY

#R2 represents the proportion of variance in the outcome variable y that may be 
#predicted by knowing the value of the x variables. An R2 value close to 1 indicates 
#that the model explains a large portion of the variance in the outcome variable.

#A problem with the R2 is that it will always increase when more variables are
#added to the model, even if those variables are only weakly associated with the
#response. A solution is to adjust the R2 by taking into account the number of 
#predictor variables.

#The adjustment in the “Adjusted R Square” value in the summary output is a 
#correction for the number of x variables included in the prediction model.

#In our example, with present price and Kilometers driven predictor variables, 
#the adjusted R2 = 0.83, meaning that “83% of the variance in the measure of 
#Selling price can be predicted by present price and Kilometers driven.

#The RSE estimate gives a measure of error of prediction. The lower the RSE, the
#more accurate the model.

#The error rate can be estimated by dividing the RSE by the mean outcome variable:
RSE <- sigma(Selling_Price)
RSE

ERROR_RATE = RSE/mean(CAR_4$Selling_Price)
ERROR_RATE
#The RSE is 2 corresponding to 44% error rate which is quite high.

#ASSUMPTION 3,4,5 - HOMOSCEDASTICITY/NORMALLY DISTRIBUTED ERRORS/INDEPENDENT ERRORS
par(mfrow=c(2,2))
plot(Selling_Price)
par(mfrow=c(1,1))

#PREDICT THE MODEL
PREDICTION <- predict(Selling_Price, newdata = test)

PREDICTION <- as.data.frame(PREDICTION)

library(Metrics)
rmse(test$Selling_Price,PREDICTION$PREDICTION)

#The lower values of RMSE indicate better fit. RMSE is a good measure of how 
#accurately the model predicts the response.










