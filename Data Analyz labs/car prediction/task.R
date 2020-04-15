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
library(Matrix)
library(MatrixModels)
library(xgboost)
library(readxl)

train <- read_excel("~/Downloads/task1 (1).xlsx", 
                    sheet = "TRAIN")
Sys.setlocale(,"ru_RU")
data <- train
View(data)

data <- data.frame(data)
data <- data %>% mutate_if(is.character,as.factor)
str(data)

#Missforest
m <- missForest(data)
data <- m$ximp
data$ID <- NULL

data <- na.omit(data)
sum(is.na(data))
#Divide
index <- createDataPartition(data$ESTIM_COST,p=0.8,list = F)
train <- data[index,]
test <- data[-index,]


#Linear regression
lm <- lm(ESTIM_COST ~ . ,train)
summary(lm)

View(test)

pred <- (predict(lm,test))
check(lm,train)

train$ESTIM_COST <- log(train$ESTIM_COST+1)
train$AVG_COST <- log(train$AVG_COST+1) 

train$ESTIM_COST <- exp(train$ESTIM_COST)-1
train$AVG_COST <- exp(train$AVG_COST)-1 

View(train)
train$as <- pred

### 2 Step
step(lm)
lm2 <- lm(ESTIM_COST ~ (YEAR + VIN_1 + VIN_2 + VIN_3 + VIN_15 + 
     FUEL_TYPE + BODY_TYPE + TYPE_OF_DRIVE + AUTO_CONDITION + 
     AVG_COST)^2, train)
summary(lm2)
check(lm2,train)




RMSE(train$ESTIM_COST,pred)


boxplot((train$ESTIM_COST))
histogram(log(data$ESTIM_COST+1))
histogram(log(data$AVG_COST+1))

confusionMatrix(test$ESTIM_COST,pred)

##Random Forest
rf <- randomForest(ESTIM_COST~.,train,
                   type="regression",
                   ntree=10,
                   do.trace=F)

check(rf,train)

pred <- exp(predict(rf, train))-1  
RMSE(test$ESTIM_COST,pred)

View(train)
  
train$ESTIM_COST <- log(train$ESTIM_COST+1)
train$AVG_COST <- log(train$AVG_COST+1) 

train$ESTIM_COST <- exp(train$ESTIM_COST)-1
train$AVG_COST <- exp(train$AVG_COST)-1 


#!!!!!! XGBOOST
train_matrix <- data.matrix(select(train,-ESTIM_COST))
test_matrix <- data.matrix(select(test,-ESTIM_COST))

train_target <- train$ESTIM_COST
test_target <- test$ESTIM_COST

dtrain <- xgb.DMatrix(data=train_matrix,label=train_target)
ctest <- xgb.DMatrix(data=test_matrix,label=test_target)

watchlist <- list(train=dtrain,test=ctest)
xgb <- xgb.train(data=dtrain,
                 nround=50,
                 maximize = FALSE,
                 early_stopping_rounds = 5,
                 watchlist = watchlist,
                 max_depth=6,
                 eval_metric = "rmse",
                 alpha=0.01,
                 lambda=0.01,
                 colsample_bytree=0.7,
                 subsample = 0.7
)
pred_xgb <- predict(xgb,ctest)

RMSE(test_target,pred_xgb)




##Functions
check <- function(x,data_file){
  lm <- lm((x$residuals)^2~.,data_file)
  print((summary(lm))$adj.r.squared)
}

