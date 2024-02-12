#1st WORK_decision tree

#Load the libraries that we need 
library(rpart)
library(datasets)
library(rpart.plot)
library(caret)
#The iris dataset is inside of the datasets library
DF <- iris

#Since the Decision Tree is a Non-statistical approach it makes no assumptions of the training data or prediction residuals

#We want to classify each observation as a certain species using the predictor variables in the dataset
#using a decision tree

#Let us make a full classification tree
FULL_TREE <- rpart(Species ~ ., method = "class", control=rpart.control(cp=0.0), minsplit=2, data=DF)

#cp is the complexity parameter which we make equal to 0
#these conditions makes sure that our tree is completely pure down to the end

#Let's plot the tree
rpart.plot(FULL_TREE)

#We can see from the root node that the data has 3 classes for the variable species, 33% in each class
#Let us check if this is true
table(DF$Species)

#Yes, it is correct

#if petal length is less than 2.5 the observation goes to the left branch, if it is greater than 
#or equal to 2.5 the observation goes to the right branch
#All the observations that had petal length less than 2.5 was classified as the species SETOSA
#The terminal node is indicating that it classified all 50 observations correctly.
#The observations that that had petal length not less than 2.5 were further split into another two branches
#if the petal width of the remaining observations is less than 1.8 they are classified as the species 
#VERSICOLOR. But this time there was some error in the classification. 91 of that branch was classified 
#correctly and the remaining 9% were classified incorrectly. 36% of the data was classified as this species.
#The remaining observations that had a petal width not less than 1.8 was classified as species VIRGINICA
#98% of the observations were classified correctly and the remaining 2% was not. 31% of the data was classified 
#as this species.

#Let's plot the complexity parameter to see if we chose the correct value
plotcp(FULL_TREE)

#The relative error is at its lowest at cp=0 which corresponds to a tree of size three splits

#SPLIT DATA INTO A TRAINING SET AND A TEST SET
#SET THE SEED
set.seed(1)

#use 80% of the data as training set and the remaining 20% as the test set
sample <- sample(c(TRUE,FALSE), nrow(DF), replace=TRUE, prob = c(0.8,0.2))
train <- DF[sample,]
test <- DF[!sample,]

#TRAIN THE MODEL
TRAIN_TREE <- rpart(Species ~ ., method = "class", control=rpart.control(cp=0.0), minsplit=2, data=train)
rpart.plot(TRAIN_TREE)
summary(TRAIN_TREE)

#Predict the model using test set
PREDICTION <- predict(TRAIN_TREE, type = "class" , test)

#Confusion matrix
confusionMatrix(test$Species, PREDICTION)



####################################################################################################

#SECOND WORK_K-means clustering

#install packages
install.packages("ggfortify")
install.packages("NbClust")

#librarys
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(NbClust)

#read in the dataset
DF_1 <- iris

#K-means clustering is an unsupervised learning model.
#Therefore we remove the Species variable from the dataset
DF_2 <- DF_1[-c(5)]

#Check what is the optimal number of clusters you need for this dataset
NbClust(DF_2, method = "complete", index = "all")$Best.nc

K.max <- 10
data  <- DF_2
wss   <- sapply(1:K.max, function(k){kmeans(data,k, nstart = 50, iter.max=15)$tot.withinss})

plot(1:K.max, wss,
     type="b" , pch=19,frame=FALSE,
     xlab="Number of cluster K",
     ylab="Total within-cluster sum of squares")

#Check the number of species types
table(DF_1$Species)

#Perform k-means clustering with 3 clusters
CLUSTERS <- kmeans(DF_2, 3)

#evaluate the clusters by looking at the cluster plot
autoplot(CLUSTERS, DF_2, frame=TRUE)
#The clusters are distinct. There appears to be no overlapping.

#evaluate the clusters by checking the cluster centres
CLUSTERS$centers
#We can see the centers of each variable are unique for each cluster, meaning that they are not overlapping.

#Check the size of the clusters
CLUSTERS$size

#make table of actual vs cluster
table(DF_1$Species, CLUSTERS$cluster)

DF_1$ACTUAL_CLUSTER <- ifelse(DF_1$Species == "virginica" ,1,
                              ifelse(DF_1$Species == "setosa", 2,
                                     ifelse(DF_1$Species == "versicolor", 3, NA)))

#Create a confusion matrix
DF_1$ACTUAL_CLUSTER <- as.factor(DF_1$ACTUAL_CLUSTER)
CLUSTERS$cluster <- as.factor(CLUSTERS$cluster)

confusionMatrix(DF_1$ACTUAL_CLUSTER, CLUSTERS$cluster)

