library(PerformanceAnalytics)
library(rpart)
library(randomForest)
library(ROSE)
library(car)
library(caret)
library(lubridate)
library(corrplot)
library(psych)
library(dplyr)
library(missForest)
library(xgboost)

setwd("~/Desktop/House")

##TRAIN DATA
data <- read.csv("train.csv")

data <- data[,-c(7,9,10,12,23,72:76)]
data <- na.omit(data)
#missforest
d <- missForest(data)
df <- d$ximp
d$OOBerror
sum(is.na(df))

#write.csv(df, "train_miss.csv", row.names=FALSE)
df <- read.csv("train_miss.csv")
df <- df[,-c(7,9,10,12,23,72:76)]
df <- data

View(data)
##TEST DATA
test_data <- read.csv("test.csv")
test_data <- test_data[,-c(7,9,10,12,23,72:76)]

sum(is.na(test_data))


#missforest
d <- missForest(test_data)
test_data <- d$ximp
d$OOBerror
sum(is.na(df_test))

df_test <- test_data

write.csv(test_data, "test_miss.csv", row.names=FALSE)

df_test <- read.csv("test_miss.csv")
df_test <- df_test[,-c(7,9,10,12,23,72:76)]


#divide into data
index <- createDataPartition(df$SalePrice, p= 0.8, list = F)
train <- df[index,]
test <- df[-index,]


#!!!!!! LM
lm <- lm(SalePrice ~.,train)         
checkData(lm)




step(lm1,direction = "forward")

lm1 <- lm(SalePrice ~ (MSSubClass  + LotArea + Neighborhood + OverallQual + 
              OverallCond  + BsmtQual + BsmtExposure + X2ndFlrSF*LotArea + 
              FullBath + KitchenQual + Fireplaces + GarageCars + ScreenPorch),train)
checkData(lm1)
saveData(lm1)
summary(lm1)

a <- test
a$pr <- predict(rf1,test)
View(a[,c(71,72)])

a["OverallCond"]




lm2 <- lm(SalePrice ~ (MSSubClass + LotArea + 
                        + LotConfig + Neighborhood + Condition1 + 
                         OverallQual + OverallCond + YearBuilt + BsmtQual +
                         BsmtExposure + BsmtFinType1+ X2ndFlrSF  + FullBath + 
                         KitchenQual + Functional + Fireplaces + GarageCars + WoodDeckSF + 
                         ScreenPorch + SaleCondition)^2,train)
checkData(lm2)
summary(lm2)

saveData(lm2)



print(lm)

lm5 <-  lm(SalePrice~(MSSubClass*LotArea + LotArea*OverallQual + 
                        OverallCond*LotArea + YearBuilt )^2, train)
checkData(lm5)
saveData(lm5)

summary(lm5)

##!!!!!!!!!!!!!!!!!
rf <- randomForest(SalePrice~(MSSubClass*LotArea + LotArea*OverallQual + 
                                OverallCond*LotArea + YearBuilt )^2,train,
                   type="classification",
                   ntree=10,
                   do.trace=F)
checkData(rf)
saveData(rf)

rf1 <- randomForest(SalePrice~.,train,
                   type="regression",
                   ntree=100,
                   do.trace=T)
checkData(rf1)

saveData(rf1)
predict(rf1,df_test)


##!!!!!SAVE DATA
saveData <- function(x){
  df_test$SalePrice <- predict(x,df_test)
  
  new_data <- data.frame(df_test[,"Id"])
  new_data$SalePrice <- df_test[,"SalePrice"]
  
  write.csv(new_data, "myresult.csv", row.names=FALSE)
}
View(df_test)


checkData <- function(x){
  pred <- predict(x,test)
  print(RMSE(test$SalePrice,pred))
  print((summary(x))$adj.r.square)
}


