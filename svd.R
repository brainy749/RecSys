## Working on SVD 

#setwd
setwd("~/Desktop/Prakash/Recsys")


# Load libraries 
library(dplyr)
library(sqldf)
library(lubridate)
library(tidyr)
library(zoo)
library(data.table)

# Load Data
FinalData <- fread("FinalDataFreeze1.csv")
FinalData <- select(FinalData,-1)

FinalData$TimeStamp<-as.Date(as.POSIXct(FinalData$TimeStamp, origin="1970-01-01"))
# Maximum and Minimum time in the dataset
maxtime <- max(FinalData$TimeStamp)
mintime <- min(FinalData$TimeStamp)
Totaltime <- as.integer(maxtime-mintime)

# Divide the data into training data and testing data
training_data <- filter(FinalData,TimeStamp < "2014-01-01")
testing_data <- filter(FinalData, TimeStamp >= "2014-01-01")
#write.csv(testing_data,"testingdata.csv")

#Unique Users and Products in training_data
length(unique(training_data$Users)) # 282
length(unique(training_data$Product)) # 124 

#Unique Users and Products in testing_data
length(unique(testing_data$Users)) # 252
length(unique(testing_data$Product)) #130

#Spread the data 
#removing the time from the training_data frame 
training_data <- select(training_data,c(1,2,3))
spreaddata <- spread(training_data,Product,Ratings)
rownames(spreaddata) <- spreaddata$Users
spreaddata <- select(spreaddata,-1)

# Collabrative Filtering techiniques 
Users <- unique(training_data$Users)
Products <- unique(training_data$Product)

# Density of the data
Density = 9325/(282*124) # 0.2666


# Create another dataframe of the samre as spreaddata
Predictions <- spreaddata


## SVD on ratingdummydata 
library(zoo)
spreaddata <- na.aggregate(spreaddata)

spreaddata_str <- spreaddata - rowMeans(spreaddata)
rowmeans <- rowMeans(spreaddata)

# Decomposing the orginal dataframe into USV format

## Taking all the eigen Values into consideration 
s1 <- svd(spreaddata_str)$d
u1 <- svd(spreaddata_str)$u
v1 <- t(svd(spreaddata_str)$v)
s1 <- diag(s1)

for(i in 1:length(spreaddata)){
  for(j in 1:length(spreaddata)){
    if (is.na(Predictions[i,j])){
      Predictions[i,j] <- rowmeans[i]+ u1[i,] %*% (t(s1))^0.5 %*% (s1)^0.5 %*% v1[,j]
    }
    else {
      Predictions[i,j] <- NA
    }
  }
}

Users <- rownames(Predictions)
Predictions <- cbind(Users, Predictions)
Predictedratings <- gather(Predictions,Product,Ratings,-Users)
write.csv(Predictedratings,"Predictions_svd.csv")



# Metrics Calculations 
# Rank-Score 
# 1. First sort each individual User Predictions in the decreasing order of ratings and Provide a rank for 
# each product
# 2. Give 1, if the Item is Present in Test Data set. Give 0, if the Item is not present in test Data Set.
# 3. Calculate (1/2^(rank-1)/alpha) for each 1 in the dataset and add them .
# 4. Take the average of all the users 

# Taking the top 20 for each user -- from Predictions 
Predictedratings <- na.omit(Predictedratings)
Predictedratings<-Predictedratings[order(Predictedratings$Users,Predictedratings$Ratings,decreasing = TRUE),]
Predictedratings <- Predictedratings %>% group_by(Users) %>% mutate(rank_pre = seq_len(n()))
Predictedratings <- Predictedratings[Predictedratings$rank_pre <= 20 ,] #Need only those rows with rank <= 20 


# Taking the top 20 products of each user -- from testing_data
testing_data_1 <- testing_data[order(testing_data$Users,testing_data$TimeStamp),]
testing_data_1 <- testing_data_1 %>% group_by(Users) %>% mutate(rank = seq_len(n()))
testing_data_1  <- testing_data_1[testing_data_1$rank <= 20 ,]


Rank_Score <- Reduce(function(x, y) merge(x, y, all=T, by=c("Users","Product")),
                     list(testing_data_1,Predictedratings))
Rank_Score <- Rank_Score[order(Rank_Score$Users,Rank_Score$rank,Rank_Score$rank_pre),]

# Calculating Rank_Score # Creating #2 as explained above
#1 Sigma(i,j) == give 1 if user i accessed item j in the test_set and 0 otherwise 
Rank_Score <- mutate(Rank_Score ,Sigma = ifelse(is.na(Ratings.y),0,1))
Rank_Score <- na.omit(Rank_Score) # Remove the rows with NA 
Rank_Score <- mutate(Rank_Score, rank_score = Sigma/(2^((rank_pre-1)/10))) #alpha = 10
Rank_Score <- Rank_Score %>% group_by(Users) %>% summarise(Avg_rank_score = mean(rank_score))
mean(Rank_Score$Avg_rank_score) #  0.5620898

# Lift Index
# 1. Divide the Predicted Dataset into 10 Parts and check how many Predictions in each decile are present in 
# test dataset.
# 2. Multiply each number in decile by Weights and divide by total number of Predictions present.
Predictedratings <- gather(Predictions,Product,Ratings,-Users)
Predictedratings_LiftIndex <- Predictedratings[order(Predictedratings$Users,Predictedratings$Ratings,decreasing = TRUE),]
Predictedratings_LiftIndex <- na.omit(Predictedratings_LiftIndex)
Predictedratings_LiftIndex <- Predictedratings_LiftIndex %>% group_by(Users) %>% mutate(quartile = ntile(Ratings, 10))

Lift_Index <- Reduce(function(x, y) merge(x, y, all=T, by=c("Users","Product")),
                     list(testing_data,Predictedratings_LiftIndex))
Lift_Index <- select(Lift_Index,-4)
Lift_Index <- na.omit(Lift_Index)
Lift_Index <- Lift_Index %>% group_by(quartile) %>% summarise(S = n()) # Number of Items in each Quartile 
Slift <- ((1*Lift_Index$S[10])+(0.9*Lift_Index$S[9])+(0.8*Lift_Index$S[8])+(0.7*Lift_Index$S[7])+
            (0.6*Lift_Index$S[6])+(0.5*Lift_Index$S[5])+(0.4*Lift_Index$S[4])+(0.3*Lift_Index$S[3])+(0.2*Lift_Index$S[2])+
            (0.1*Lift_Index$S[1]))/sum(Lift_Index$S) # 0.5663136


# RMSE
# 1. Take the difference of Prediction - Actual  and square and add each value and divide by total number of 
# Observation 
Predictedratings <- gather(Predictions,Product,Ratings,-Users)
RMSE <- Reduce(function(x, y) merge(x, y, all=T, by=c("Users","Product")),
               list(Predictedratings,testing_data))
RMSE <- select(RMSE,-5) # Remove time 
RMSE <- na.omit(RMSE)
RMSE <- mutate(RMSE, SQUARE = (Ratings.x - Ratings.y)^2)
sum(RMSE$SQUARE)/nrow(RMSE) #  0.5637429

# Correlation 
# 1. Find the correlation between Prediction and Actual values
Predictedratings <- gather(Predictions,Product,Ratings,-Users)
Correlation <- Reduce(function(x, y) merge(x, y, all=T, by=c("Users","Product")),
                      list(Predictedratings,testing_data))
Correlation <- select(Correlation,-5) # Remove time 
Correlation <- na.omit(Correlation)
correlation <- cor(Correlation$Ratings.x,Correlation$Ratings.y) #0.2159523



# All the Metrics calculated 

