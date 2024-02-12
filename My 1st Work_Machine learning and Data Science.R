

#read in the data
AUTO <- read.csv(auto-mpg.csv)
AUTO <- read.csv("auto-mpg.csv")
AUTO <- read.csv("AUTO-MPG.CSV")
AUTO = read.csv("AUTO-MPG.CSV")

AUTO1 <- read.csv("auto-mpg-R1.csv")
AUTO2 <- read.csv("auto-mpg-R2.csv")

#install SQLDF package
install.packages("sqldf")

#load library
library("sqldf")

#create an ID variable in both data sets
AUTO1 <- cbind(ID = 1:nrow(AUTO1), AUTO1)
AUTO2 <- cbind(ID = 1:nrow(AUTO2), AUTO2)

#Names of variables in data sets
names(AUTO1)
names(AUTO2)

#rename variables in data frame
colnames(AUTO2)[4] = "model_year"
colnames(AUTO2)[6] = "car_name"

#inner join the two tables using SQL
AUTO3 <- sqldf("SELECT a.ID, a.mpg, a.cylinders, a.displacement, a.horsepower,
                       
                       b.weight, b.acceleration, b.model_year, b.origin, b.car_name
                
                FROM   AUTO1 as a, AUTO2 as b
                
                WHERE  a.ID=b.ID")


############################################################################################

#SECOND WORK_ PRACTICAL 2


# CREATE A VECTOR OF NAMES
NAME <- c("JANE", "JACK", "JILL", "JOHN", "JOE", "JOE")

# CREATE A VECTOR OF DATE OF BIRTHS
DOB <- c("05/27/84", "07/07/05"," ", "07/14/09", "12/22/22", "12/22/22")

# CREAT A VECTOR OF WEIGHTS
WEIGHT <- c(89.23, 74.37, 69.56, 82.34, 92.82, 92.82)

# CREAT A VECTOR OF HEIGHTS
HEIGHT <- c(174, 150, 163, 171, 189, 189)

# SET VECTORS AS A DATA FRAME
NAME <- as.data.frame(NAME)
DOB <- as.data.frame(DOB)
WEIGHT <- as.data.frame(WEIGHT)
HEIGHT <- as.data.frame(HEIGHT)

# COMBINE THE TWO DATA FRAMES
DF <- cbind(DOB,NAME,WEIGHT,HEIGHT)

# GET RID OF DUBLICATES
install.packages(dplyr) 
library(dplyr)
install.packages(dplyr)

DF <- distinct(DF)


#ROUNDING NUMERICAL VARIABLES
class(DF$WEIGHT)

DF$WEIGHT <- round(DF$WEIGHT,1)
DF$WEIGHT <- round(DF$WEIGHT,0)

#SUMMARY STATISTICS OF NUMERICAL VARIABLES
summary(DF$WEIGHT)

# CEATE A CATEGORICAL VARIABLE USING IF_ELSE
DF$WEIGHT_TYPE <- if_else(DF$WEIGHT>70 & DF$HEIGHT<170,"OVERWEIGHT","NORMAL")

# CHECK THE CLASS OF THE DOB VARIABLE
class(DF$DOB)

# If your dates are stored as characters, you simply need to provide as.Date 
# with your vector of DOB and the format they are currently stored in. 
# Format dates as YYYY-MM-DD
DF$DATE <- as.Date(DF$DOB, format = "%m/%d/%y")

# CHECK THE CLASS OF THE DATE VARIABLE
class(DF$DATE)

# WHAT DAY OF THE WEEK WAS THE PERSON BORN?
#install.packages("tidyverse")
library("tidyverse")

# SUNDAY IS THE FIRST DAY OF THE WEEK AND SATURDAY IS THE 7TH DAY OF THE WEEK
DF$WEEK_DAY_NUMBER <- wday(DF$DATE)

DF$WEEK_DAY <- wday(DF$DATE, label=TRUE)

DF$WEEK_DAY_FULL_NAME <- wday(DF$DATE, label=TRUE, abbr=FALSE)

# WHICH MONTH WAS THE PERSON BORN?
DF$MONTH <- months(DF$DATE, abbr=TRUE)

DF$MONTH_FULL_NAME <- months(DF$DATE)

# AGE OF THE PERSON
#install.packages("lubridate")
library("lubridate")

# FIRST CREATE TODAYS DATE SO WE CAN SEE HOW MANY YEARS IT HAS BEEN SINCE THEN
TODAYS_DATE <- as.Date("2023-02-27")

# Operator %?% creates a time interval from the DOB to a specified date, and 
# that is divided with a 1-year period by using function years.
DF$AGE <- trunc((DF$DATE %--% TODAYS_DATE) / years(1))

# GET RID OF MISSING VALUES
DF <- na.omit(DF)

# DROP DOB COLUMN
DF <- DF[-c(1,7,8,10)]

# RE-ORDER COLUMNS
DF <- DF[, c(1,2,8,3,4,5,6,7)]

# RENAMING COLUMN
colnames(DF)[6] = "DOB"

# Filtering data
DF_1 <- as.data.frame(DF %>% filter(DOB == '1984-05-27'))

DF_2 <- as.data.frame(DF %>% filter(DOB > '1984-05-27'))

DF_3 <- as.data.frame(DF %>% filter(DOB >= '1984-05-27'))

#SPLIT DATASETS IN R
DF1 <- DF[-c(5:8)]
DF2 <- DF[-c(2:4)]

# MERGING DATASETS WITHOUT SQL
DF3 <- merge(DF1, DF2, by.x = "NAME", by.y = "NAME")

# MERGE USING SQL
library("sqldf")

names(DF2)

DF4 <- sqldf("SELECT DF1.*, 
                     DF2.WEIGHT_TYPE, DF2.DOB, DF2.WEEK_DAY_FULL_NAME,DF2.MONTH_FULL_NAME
                FROM DF1, DF2
               WHERE DF1.NAME = DF2.NAME")




