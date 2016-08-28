## Working on SVD 

#load the required libraries

# Set the working directory
setwd("~/Desktop/Satish patil/RecSys")

# Load the data.
ratingsdummydata <- read.csv("ratingsdummydata.csv", header = TRUE)


rownames(ratingsdummydata) <- ratingsdummydata[,1]
ratingsdummydata <- ratingsdummydata[,-1]
str(ratingsdummydata)

# Create another dataframe of the samre as ratingsdummydata
Predictions <- ratingsdummydata


## SVD on ratingdummydata 
library(zoo)
ratingsdummydata <- na.aggregate(ratingsdummydata)

ratingsdummydata_str <- ratingsdummydata - rowMeans(ratingsdummydata)
rowmeans <- rowMeans(ratingsdummydata)

# Decomposing the orginal dataframe into USV format

## Taking all the eigen Values into consideration 
s1 <- svd(ratingsdummydata_str)$d
u1 <- svd(ratingsdummydata_str)$u
v1 <- t(svd(ratingsdummydata_str)$v)
s1 <- diag(s1)

for(i in 1:length(ratingsdummydata)){
  for(j in 1:length(ratingsdummydata)){
    if (is.na(Predictions[i,j])){
      Predictions[i,j] <- rowmeans[i]+ u1[i,] %*% (t(s1))^0.5 %*% (s1)^0.5 %*% v1[,j]
    }
    else {
      Predictions[i,j] <- NA
    }
  }
}

Predictions1 <- Predictions
