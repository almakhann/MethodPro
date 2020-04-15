setwd("~/Desktop//MethodPro/Data Analyz labs/Titanic/")
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



#train Data Manipulation
train <- read.csv("train.csv")
train <- train %>% mutate_if(is.integer, as.numeric)

df <- train[,-c(1,4,9,11,12)]
str(df)

df <- na.omit(df)

#Replacing with missforest
m <- missForest(df)
df <- m$ximp
m$OOBerror
sum(is.na(df))


#TestData Manipualtion
testData <- read.csv("test.csv")
testData <- testData %>% mutate_if(is.integer, as.numeric)

df_test <- testData[,-c(1,3,8,10,11)]
#Replacing with missforest for test data
d <- missForest(df_test)
df_test <- d$ximp
d$OOBerror
sum(is.na(df_test))


df = train
##Create Data Portion
index <- createDataPartition(df$Survived,p=0.8, list = F)
train <- df[index,]
test <- df[-index,]

confusionMatrix(as.vector(test$Survived),as.vector(pred_dt))
pred_dt <- predict(lm6,test)
pred_dt <- ifelse(pred_dt > 0.5,1,0)
as.vector(pred_dt)


train <- df
#!!!!!! DECISION TREE
dt <- rpart(Survived~.,train)
checkDt(dt)

#0.73684
dt1 <- rpart(Survived ~ (Pclass + Sex + Age + SibSp + Parch), train)
checkDt(dt1)
saveData(dt1)


prop.table(table(train$Sex,train$Survived),1)





#!!!!!! RANDOM FOREST
##Result ===== 0.77990
rf <- randomForest(Survived~(Pclass+Sex*Age+SibSp+Age*Parch+Pclass*Fare)^2,train,
                       type="regression",
                       ntree=10,
                       do.trace=F)
checkDt(rf)

##Result 0.75598
rf1 <- randomForest(Survived~.,train,
                   type="classification",
                   ntree=1000,
                   do.trace=F)
checkDt(rf1)

# 0.3657907
rf2 <- randomForest(Survived~(Pclass+Sex+Age+SibSp*Parch),train,
                   type="regression",
                   ntree=1000,
                   do.trace=F)
checkDt(rf2)


saveData(rf2)







#!!!!!!!!!!!!!!! LM
lm <- lm(Survived~.,train)
summary(lm)

#to find ideal value
ideal <- step(lm,direction = "backward")

lm1 <- lm(Survived~(Pclass+Age+Sex*Age+SibSp*Fare)^2,train)
checkData(lm1)

lm0 <- lm(Survived ~ (Pclass + Sex + Age + SibSp)^2,train)
checkData(lm0)

lm2 <- lm(Survived~(Pclass+SibSp*Parch+Age*Fare+Sex*Fare)^2,train)
checkData(lm2)

lm3 <- glm(Survived~(Pclass+Age+Sex*Age+SibSp)^3,train, family = "binomial")
checkData(lm3)

##0.78468
lm4 <- lm(Survived~(Pclass*Sex+Sex*Age+Age*Fare+SibSp*Parch)^2,train)
checkData(lm4)

#0.38
lm5 <- lm(Survived~(Pclass*Sex+Sex*Age+SibSp*Parch)^2,train)
checkData(lm5)

###TOP RESULT 0.78947
lm6 <- lm(Survived~(Pclass*Sex+Sex*Age+SibSp*Parch)^3,train)
checkData(lm6)

lm7 <- glm(Survived~(Pclass+Sex+Parch+Age+Parch*SibSp)^3,train,family= "binomial")
checkData(lm7)

saveData(lm6)


step(lm7,direction = "backward")

##0.78468
lm8 <- lm(formula = Survived ~ (Pclass + Sex + Age + SibSp + Parch)^3,train)
checkData(lm8)

saveData(lm8)






##
train$Sex <- as.numeric(train$Sex)
chart.Correlation(train)


#!!!!!! XGBOOST
train_matrix <- data.matrix(select(train,-Survived))
test_matrix <- data.matrix(select(test,-Survived))

train_target <- train$Survived
test_target <- test$Survived

dtrain <- xgb.DMatrix(data=train_matrix,label=train_target)
ctest <- xgb.DMatrix(data=test_matrix,label=test_target)

watchlist <- list(train=dtrain,test=ctest)
xgb <- xgb.train(data=dtrain,
                 nround=50,
                 maximize = FALSE,
                 early_stopping_rounds = 5,
                 watchlist = watchlist,
                 max_depth=6,
                 #objective = "reg:linear",
                 eval_metric = "rmse",
                 alpha=0.01,
                 lambda=0.01,
                 colsample_bytree=0.7,
                 subsample = 0.7
)
pred_xgb <- predict(xgb,ctest)
RMSE(test_target,pred_xgb)
all_testData <- df_test

##0.77
pred_xgb <- predict(xgb, all)
#testing this xgboost
all_testData$a <- 5 
all_testData$a <- as.numeric(all_testData$a)

all <- data.matrix(select(all_testData,-a,Survived))
all
test$Surviv <- pred_xgb
View(test)



#!!!!!!Saving Data
saveData <- function(x){
  df_test$Survived <- predict(x,df_test)
  df_test$Survived <- ifelse(df_test$Survived >= 0.5,1,0)

  new_data <- data.frame(testData[,"PassengerId"])
  new_data$Survived <- df_test[,"Survived"]
  write.csv(new_data, "myresult.csv", row.names=FALSE)
}

##!!!!!!Checker
checkData <- function(x){
  pred <- predict(x,test)
  print (RMSE(test$Survived,pred))
  
  pred <- predict(x,test)
  pred <- ifelse(pred >= 0.5,1,0)
  print(RMSE(test$Survived,pred))
  paste0("Adj Rsquare: ", (summary(x))$adj.r.squared)
}

checkDt <- function(x){
  pred <- predict(x,test)
  print (RMSE(test$Survived,pred))
  
  pred <- predict(x,test)
  pred <- ifelse(pred >= 0.5,1,0)
  print(RMSE(test$Survived,pred))
}



