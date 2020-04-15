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

##Train Data
train <- read.csv("train.csv")

train <- train %>% mutate_if(is.integer,as.numeric)


nums <- unlist(lapply(train, is.numeric))  
t <- train[nums]


d <- missForest(t)
df <- d$ximp
d$OOBerror
sum(is.na(df))


##TEST
test <- read.csv("test.csv")
test <- test %>% mutate_if(is.integer,as.numeric)
nums <- unlist(lapply(test, is.numeric))  
test_df <- test[nums]

d <- missForest(test_df)
testing <- d$ximp
d$OOBerror

index <- createDataPartition(df$SalePrice, p=0.8,list = F)
train <- df[index,]
test <- df[-index,]

###RandomForest
rf <- randomForest(SalePrice ~., train,
                   type="classification",
                   ntree=100,
                   do.trace=F)
checkData(rf)

rf1 <- randomForest(SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + 
                      OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + 
                      BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BsmtFullBath + FullBath + 
                      BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + 
                      GarageCars + WoodDeckSF + X3SsnPorch + ScreenPorch , train,
                   type="regression",
                   ntree=1000,
                   do.trace=F)
checkData(rf1)



###LM
lm <- lm(SalePrice~.,train)
checkData(lm)

step(lm,direction = "backward")

lm1 <- lm(SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + 
           OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + 
           BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BsmtFullBath + FullBath + 
           BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + 
           GarageCars + WoodDeckSF + X3SsnPorch + ScreenPorch,train)
checkData(lm1)


###XGBoost
train_matrix <- data.matrix(select(train,-SalePrice))
test_matrix <- data.matrix(select(test,-SalePrice))

train_target <- train$SalePrice
test_target <- test$SalePrice

dtrain <- xgb.DMatrix(data=train_matrix,label=train_target)
ctest <- xgb.DMatrix(data=test_matrix,label=test_target)

watchlist <- list(train=dtrain,test=ctest)
xgb <- xgb.train(data=dtrain,
                 nround=1000,
                 maximize = FALSE,
                 early_stopping_rounds = 10,
                 watchlist = watchlist,
                 max_depth=4,
                 #objective = "reg:linear",
                 eval_metric = "rmse",
                 alpha=0.05,
                 lambda=0.05,
                 colsample_bytree=0.7,
                 subsample = 0.7,eta
)
pred_xgb <- predict(xgb,ctest)
RMSE(test_target,pred_xgb)
all_testData <- df_test

sum(is.na(train_matrix))

pred_xgb <- predict(xgb, testing)
#testing this xgboost
a <- data.matrix(select(testing,-SalePrice))
testing$SalePrice <- predict(xgb,a)




##Saving Data
saveData <- function(x){
  testing$SalePrice <- predict(rf,testing)
  new_data <- data.frame(testing[,"Id"])
  new_data$SalePrice <- testing[,"SalePrice"]
  write.csv(new_data, "myresult.csv", row.names=FALSE)
}

checkData <- function(x){
  pred <- predict(x,test)
  print(RMSE(test$SalePrice,pred))
  print((summary(x))$adj.r.square)
}
