#setwd
setwd("~/Desktop/Prakash/RecSys")
library(dplyr)
library(sqldf)
library(lubridate)
library(tidyr)
library(zoo)
library(data.table)


# Load the Data
data <- fread("ratings_Books.csv")
str(data)

#Set Colnames
colnames(data) <- c("Users","Product","Ratings","TimeStamp")

# Convert time to as.POSIX factor
#data$TimeStamp<-as.Date(as.POSIXct(data$TimeStamp, origin="1970-01-01"))


# Select Products with count(Rating) > 20 
Product20 <- data[.Rating>20,]

# Total Number of Unique Users 
UniqueUsers <- length(unique(data$Users))

# Total Number of Unique Products
UniqueProducts <- length(unique(data$Product))

# Maximum and Minimum time in the dataset
maxtime <- max(data$TimeStamp)
mintime <- min(data$TimeStamp)
Totaltime <- as.integer(maxtime-mintime)

#RF Analysis 
# Recency and Frequency
#Frequency of Users 
FrequencyUser = sqldf("select Users, count(Users) as Frequency from data group by Users")


#Recency of Users
RecencyUser = data %>% group_by(Users) %>% summarise(lastpurchasedate = max(TimeStamp))
maxdate = max(data$TimeStamp)+1
RecencyUser = mutate(RecencyUser, Recency = maxdate - lastpurchasedate)
RecencyUser = RecencyUser[,c(1,3)]
RecencyUser$Recency <- as.numeric(RecencyUser$Recency)


#Frequency of Products
FrequencyProduct = sqldf("select Product, count(Product) as Frequency from data group by Product")


#Recency of Products 
RecencyProduct = data %>% group_by(Product) %>% summarise(lastpurchasedate = max(TimeStamp))
maxdate = max(data$TimeStamp)+1
RecencyProduct = mutate(RecencyProduct, Recency = maxdate - lastpurchasedate)
RecencyProduct = RecencyProduct[,c(1,3)]
RecencyProduct$Recency <- as.numeric(RecencyProduct$Recency)


# Joining Recency and frequency of product 
RF_Product <- sqldf("select FP.Product,Frequency,Recency from FrequencyProduct as FP, RecencyProduct as RP where FP.Product = RP.Product")
RF_Users <- sqldf("select FU.Users,Frequency,Recency from FrequencyUser as FU, RecencyUser as RU where FU.Users = RU.Users")

#Clustering the Users 
RFscore = RF_Users
rownames(RFscore) <- RFscore[,1]
RFscore <- RFscore[,-1]
RFscore <- scale(RFscore)

Kmeans_result <- kmeans(RFscore,centers = 4)
cluster = Kmeans_result$cluster
RF_Users_final <- cbind(RF_Users,cluster)

#Clustering the Products
RFscore = RF_Product
rownames(RFscore) <- RFscore[,1]
RFscore <- RFscore[,-1]
RFscore <- scale(RFscore)

Kmeans_result <- kmeans(RFscore,centers = 4)
cluster = Kmeans_result$cluster
RF_Product_final <- cbind(RF_Product,cluster)


# Assumption
# 1. Users who are active only in the last 6 months were being taken into consideration. All other customers
# Were considered dead(abondend). This is to make computations very fast and not get optimized results 

Cuttoffdate <- maxdate-180
RecentUsers <- filter(data, TimeStamp>=Cuttoffdate)
length(unique(RecentUsers$Users)) #1973921
length(unique(RecentUsers$Product)) # 761088

# Frequecy of the Users 
FrequencyofUser = sqldf("select Users, count(Users) as Frequency from RecentUsers group by Users")

# Frequecy of the Products 
FrequencyofProduct = sqldf("select Product, count(Product) as Frequency from data group by Product")


# We are picking up the top 5000 customers and top 1000 Products.
# We are doing this to make computations possible on low end systems and test our Algorithms.
Top5000_customers = sqldf("select Users from FrequencyofUser FU order by Frequency desc limit 5000")
Top1000_Product = sqldf("select Product from FrequencyofProduct order by Frequency desc limit 1000")


# Lame way of freezing data.

#  Extract the Products and Users from our selected list from the original data base.
FinalData1 <- sqldf("select * from data group by Product having count(Ratings) > 40")
FinalData <- sqldf("select * from data where Product in (select Product from FinalData1)")


UsersGreater20 <- sqldf("select * from FinalData group by Users having count(Ratings)>40")
FinalData2 <- sqldf("select * from FinalData where Users in (select Users from UsersGreater20)") #

ProductsGreater20 <- sqldf("select * from FinalData2 group by Product having count(Ratings) > 40")
FinalData3 <- sqldf("select * from FinalData2 where Product in (select Product from ProductsGreater20)")
length(unique(ProductsGreater20$Product))
length(unique(ProductsGreater20$Users))

length(unique(FinalData3$Product))
length(unique(FinalData3$Users))


UsersGreater20_2 <-  sqldf("select * from FinalData3 group by Users having count(Ratings)>40")
FinalData4 <- sqldf("select * from FinalData3 where Users in (select Users from UsersGreater20_2)") #
length(unique(UsersGreater20_2$Product))
length(unique(UsersGreater20_2$Users))

length(unique(FinalData4$Product))
length(unique(FinalData4$Users))


ProductsGreater20_2 <- sqldf("select * from FinalData4 group by Product having count(Ratings) > 40")
FinalData5 <- sqldf("select * from FinalData4 where Product in (select Product from ProductsGreater20_2)")
length(unique(ProductsGreater20_2$Product))
length(unique(ProductsGreater20_2$Users))

length(unique(FinalData5$Product))
length(unique(FinalData5$Users))


UsersGreater20_3 <-  sqldf("select * from FinalData5 group by Users having count(Ratings)>40")
FinalData6 <- sqldf("select * from FinalData5 where Users in (select Users from UsersGreater20_3)") #
length(unique(UsersGreater20_3$Product))
length(unique(UsersGreater20_3$Users))

length(unique(FinalData6$Product))
length(unique(FinalData6$Users))


ProductsGreater20_3 <- sqldf("select * from FinalData6 group by Product having count(Ratings) > 40")
FinalData7 <- sqldf("select * from FinalData6 where Product in (select Product from ProductsGreater20_3)")
length(unique(ProductsGreater20_3$Product))
length(unique(ProductsGreater20_3$Users))

length(unique(FinalData7$Product))
length(unique(FinalData7$Users))



UsersGreater20_4 <-  sqldf("select * from FinalData7 group by Users having count(Ratings)>40")
FinalData8 <- sqldf("select * from FinalData7 where Users in (select Users from UsersGreater20_4)") #
length(unique(UsersGreater20_4$Product))
length(unique(UsersGreater20_4$Users))

length(unique(FinalData8$Product))
length(unique(FinalData8$Users))

ProductsGreater20_4 <- sqldf("select * from FinalData8 group by Product having count(Ratings) > 40")
FinalData9 <- sqldf("select * from FinalData8 where Product in (select Product from ProductsGreater20_4)")
length(unique(ProductsGreater20_4$Product))
length(unique(ProductsGreater20_4$Users))

length(unique(FinalData9$Product))
length(unique(FinalData9$Users))


UsersGreater20_5 <-  sqldf("select * from FinalData9 group by Users having count(Ratings)>40")
FinalData10 <- sqldf("select * from FinalData9 where Users in (select Users from UsersGreater20_5)") #
length(unique(UsersGreater20_5$Product))
length(unique(UsersGreater20_5$Users))

length(unique(FinalData10$Product))
length(unique(FinalData10$Users))


ProductsGreater20_5 <- sqldf("select * from FinalData10 group by Product having count(Ratings) > 40")
FinalData11 <- sqldf("select * from FinalData10 where Product in (select Product from ProductsGreater20_5)")
length(unique(ProductsGreater20_5$Product))
length(unique(ProductsGreater20_5$Users))

length(unique(FinalData11$Product))
length(unique(FinalData11$Users))


UsersGreater20_6 <-  sqldf("select * from FinalData11 group by Users having count(Ratings)>40")
FinalData12 <- sqldf("select * from FinalData11 where Users in (select Users from UsersGreater20_6)") #
length(unique(UsersGreater20_6$Product))
length(unique(UsersGreater20_6$Users))

length(unique(FinalData12$Product))
length(unique(FinalData12$Users))

ProductsGreater20_6 <-  sqldf("select * from FinalData12 group by Product having count(Ratings)>40")
FinalData13 <- sqldf("select * from FinalData12 where Product in (select Product from ProductsGreater20_6)") #
length(unique(ProductsGreater20_6$Product))
length(unique(ProductsGreater20_6$Users))

length(unique(FinalData13$Product))
length(unique(FinalData13$Users))

UsersGreater20_7 <-  sqldf("select * from FinalData13 group by Users having count(Ratings)>60")
FinalData14 <- sqldf("select * from FinalData13 where Users in (select Users from UsersGreater20_7)") #
length(unique(UsersGreater20_7$Product))
length(unique(UsersGreater20_7$Users))

length(unique(FinalData14$Product))
length(unique(FinalData14$Users))

ProductsGreater20_7 <-  sqldf("select * from FinalData14 group by Product having count(Ratings)>60")
FinalData15 <- sqldf("select * from FinalData14 where Product in (select Product from ProductsGreater20_7)") #
length(unique(UsersGreater20_5$Product))
length(unique(UsersGreater20_5$Users))

length(unique(FinalData15$Product))
length(unique(FinalData15$Users))

UsersGreater20_8 <-  sqldf("select * from FinalData15 group by Users having count(Ratings)>60")
FinalData16 <- sqldf("select * from FinalData15 where Users in (select Users from UsersGreater20_8)") #
length(unique(UsersGreater20_8$Product))
length(unique(UsersGreater20_8$Users))


length(unique(FinalData16$Product))
length(unique(FinalData16$Users))


ProductsGreater20_8 <-  sqldf("select * from FinalData16 group by Product having count(Ratings)>60")
FinalData17 <- sqldf("select * from FinalData16 where Product in (select Product from ProductsGreater20_8)") #
length(unique(ProductsGreater20_8$Product))
length(unique(ProductsGreater20_8$Users))

length(unique(FinalData17$Product))
length(unique(FinalData17$Users))


# We have 283 users and 130 Products 
write.csv(FinalData17,"FinalDataFreeze1.csv",row.names = FALSE)

################################################

#Data FreeZed

################################################
