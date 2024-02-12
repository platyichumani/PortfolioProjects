#Read in the dataset
HEART <- read.csv("heart.csv")

#Check which variables has missing values
colnames(HEART)[apply(HEART,2,anyNA)]

#Check how many rows of data have NA's
nrow(HEART[is.na(HEART$Thal)| is.na(HEART$Ca),])

#We can view these 6 rows of NA's
HEART[is.na(HEART$Thal)| is.na(HEART$Ca),]

#We could impute the data by replacing the missing values but instead we will remove them
HEART_1 <- na.omit(HEART)

#ASSUMPTION 1 & 2 are already met
#The Response Variable is Binary / Errors are binary
#Non - Constant variance

#Assumption 3
#Independent Y Values / Errors
#Let's take this as met since we do not have the study design

#Assumption 4
#Linear Relationship between the numerical X values and the Log odds of Y
#The Box-Tidwell test is used to check for linearity between the predictors and the logit. 

#Make the dependent 0 and 1's
HEART_1$AHD <- ifelse(HEART_1$AHD == "Yes", 1, 0)

#H0:there is no linear relationship
#H1:there is a linear relationship
library(car)

boxTidwell(AHD ~ Age, data=HEART_1)

#Since the p-value is greater than 0.05 we accept the null hypothesis
#Thus, there is no linear relationship between Age and the log odds of AHD

#Let's look at Age
#The 5 number summary
summary(HEART_1$Age)

#Histogram of numerical data
hist(HEART_1$Age, xlab= "AGE (IN YEARS)" , main= "Histogram of Age", breaks="Sturges")

#Check for outliers
HEART_1$Age_ZSCORE <- scale(HEART_1$Age)

summary(HEART_1$Age_ZSCORE)

hist(HEART_1$Age_ZSCORE, xlab= "AGE Z SCORE" , main= "Histogram of Age", breaks="Sturges")

#Assumption 5 - No Extreme outliers
IQR <- IQR(HEART_1$Age)
Q1  <- 48
Q3  <- 61
C   <- 3

Tmin <- Q1 - (C*IQR)
Tmax <- Q3 + (C*IQR)

HEART_1$Age[which(HEART_1$Age < Tmin | HEART_1$Age > Tmax)]

#Box and whisker plot of numerical data
boxplot(HEART_1$Age, ylab= "AGE (IN YEARS)", main = "Boxplot of Age")

#Assumption 6 - Sample size
#The sample size should at least be 500 observations to perform a logistic regression.
#Our dataset only has 297 observations but let's continue anyway

#Assumption 7 - Multicollinearity between independent variables
#Use correlation matrix for numerical variables
#Use Chi-square test of independence for categorical variables

#Correlation matrix
#Let's find the structure of the data
str(HEART_1)

#It is a categorical variable where 0 =female and 1=male.
HEART_1[HEART_1$Sex == 0,]$Sex <- "F"
HEART_1[HEART_1$Sex == 1,]$Sex <- "M"

#The following classes are incorrect.
#The main difference between characters and factors are that factors have predefined levels. 
#Thus their value can only be one of those levels or NA. Whereas characters can be anything.
HEART_1$Sex       <- as.factor(HEART_1$Sex)
HEART_1$ChestPain <- as.factor(HEART_1$ChestPain)
HEART_1$Fbs       <- as.factor(HEART_1$Fbs)
HEART_1$RestECG   <- as.factor(HEART_1$RestECG)
HEART_1$ExAng     <- as.factor(HEART_1$ExAng)
HEART_1$Slope     <- as.factor(HEART_1$Slope)
HEART_1$Ca        <- as.factor(HEART_1$Ca)
HEART_1$Thal      <- as.factor(HEART_1$Thal)
HEART_1$AHD       <- as.factor(HEART_1$AHD)

str(HEART_1)

NUMERICAL_VARIABLES <- HEART_1[c(1,4,5,8,10)]

cor(NUMERICAL_VARIABLES)

#Chi-square test of independence

#H0: the variables are independent, there is no relationship between the two 
#categorical variables. Knowing the value of one variable does not help to 
#predict the value of the other variable

#H1: the variables are dependent, there is a relationship between the two 
#categorical variables. Knowing the value of one variable helps to predict the 
#value of the other variable

#Sex and ChestPain
library(ggplot2)

ggplot(HEART_1) + aes(x=Sex, fill = ChestPain) + geom_bar(position = "dodge")

CHISQ_SEXvsCHESTPAIN <- chisq.test(table(HEART_1$Sex, HEART_1$ChestPain))
CHISQ_SEXvsCHESTPAIN

#Since the P-value is greater than 0.05 we accept the null hypothesis
#Thus, the variables are independent, there is no relationship

#We need to make sure that each each category of the categorical variables are represented 
#in the data otherwise our data will be skewed towards one specific category which is bad 
#for the model there should be a large enough amount in each category to be representative
xtabs(~ AHD + Sex, HEART_1)
xtabs(~ AHD + ChestPain, HEART_1)
xtabs(~ AHD + Fbs, HEART_1)
xtabs(~ AHD + RestECG, HEART_1) #only 4 patients represents level 1, this could affect our model
xtabs(~ AHD + ExAng, HEART_1)
xtabs(~ AHD + Slope, HEART_1)
xtabs(~ AHD + Ca, HEART_1)
xtabs(~ AHD + Thal, HEART_1)

HEART_1 <- HEART_1[-c(15)]

#SPLIT DATA INTO A TRAINING SET AND A TEST SET
#SET THE SEED
set.seed(1)

#use 80% of the data as training set and the remaining 20% as the test set
sample <- sample(c(TRUE,FALSE), nrow(HEART_1), replace=TRUE, prob=c(0.8,0.2))
train  <- HEART_1[sample,]
test   <- HEART_1[!sample,]

#generalised linear models
LOGISTIC_1 <- glm(AHD ~ Sex, data=train , family='binomial')

summary(LOGISTIC_1)

#Advanced heart disease = -1.3049 +1.6296(Male)
#if the person is male then substitute 1
#if the patient is female the substitute 0

#if we are predicting heart disease for a female patient we get the following equation:
#Advanced heart disease = -1.3049 +1.6296(0) = -1.3049
#this means that the log(odds) that a female has heart disease is -1.3049

#if we are predicting heart disease for a male patient we get the following equation:
#Advanced heart disease = -1.3049 +1.6296(1) = 0.3247
#this means that the log(odds) that a male has heart disease is 0.3247

#The AIC = Akaike Information Criterion is used to compare models
#Lower AIC values indicate a better-fit model
#AIC = 318.44

#Now let's make the prediction using the test set

PREDICTION <- predict(LOGISTIC_1 , test, type='response')

PRED <- as.data.frame(PREDICTION)

test$PREDICT <- ifelse(PRED$PREDICTION > 0.5, "0" , "1")

class(test$PREDICT)
class(test$AHD)

test$PREDICT <- as.factor(test$PREDICT)
class(test$PREDICT)

library(caret)

confusionMatrix(test$PREDICT,test$AHD)
