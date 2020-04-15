library(readr)
library(dplyr)
library(PerformanceAnalytics)
library(rpart)
library(randomForest)
library(ROSE)
library(car)
library(caret)
library(lubridate)
library(corrplot)
library(psych)
library(missForest)
library(Matrix)
library(MatrixModels)
library(xgboost)

setwd("~/Desktop/Data Analyz labs/task2")
Sys.setlocale(,"ru_RU")

df_test <- read_csv("test_k2.csv")
Encoding(colnames(df_test)) <- "UTF-8"
View(df_test)

test_data <- df_test
test_data <- test_data %>% mutate_if(is.integer,as.numeric)
test_data <- test_data %>% mutate_if(is.character,as.factor)


test_data[] <- lapply(test_data, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})


df_train <- read_csv("train_k2.csv")
sum(is.na(df_train))

data <- df_train
data <- data %>% mutate_if(is.integer,as.numeric)
data <- data %>% mutate_if(is.character,as.factor)
                
data <- na.omit(data)
data$NUM <- NULL
           
index <- createDataPartition(data$TARGET, p=0.8,list = F)
train <- data[index,]
test <- data[-index,]

#Linear Regression
lm <- glm(as.factor(TARGET)~.,train,family = "binomial")
summary(lm)

step(lm2)
lm2 <- lm(TARGET~(F1 + F2 + F3 + F7 + F10 + F11 + F12 + F14 + 
            F15 + F16 + F17 + F19 + F20 + F21 + F22 + F23 + F24 + F26 + 
            F27 + F28 + F30 + F32 + F33 + F34 + F35 + F36 + F37 + F38 + 
            F39 + F43 + F45 + F46 + F47 + F50 + F52 + F54 + F57 + F58 + 
            F59 + F60 + F61 + F64 + F65 + F67 + F69 + F70 + F71 + F72 + 
            F73 + F80 + F81 + F82 + F83 + F84 + F85 + F89 + F94 + F96 + 
            F98 + F101 + F102 + F104 + F108 + F110 + F111 + F113 + F114 + 
            F116 + F117 + F119 + F120 + F123 + F125 + F126 + F129),train)
summary(lm2)
check(lm2,train)s
summary(lm2)


View(train)

pred_lm <- predict(lm2,test_data)
pred_lm <- ifelse(pred_lm >= 0.5 , 1, 0)

test$a <- pred_lm
View(test)
RMSE(pred_lm,test$TARGET)



sum(pred_lm == test$TARGET)
818/1079

##RandomForest
rf = randomForest(as.factor(TARGET)~.,train,
                  ntree = 100,
                  type= "regression",
                  do.trace= T)
pred <- predict(rf,test)
pred <- ifelse(pred == "0",0,1)
RMSE(pred,test$TARGET)


sum(data$TARGET == pred)
str(pred)





#Functions to check residuals error persentage
check <- function(x,data){
  lm <- lm((x$residuals)^2~.,data)
  (summary(lm))$adj.r.squared
} 

##Saving data
pred <- predict(lm,test)
pred <- ifelse(pred == "0",0,1)
test_data$TARGET <- pred_lm
  
new_data <- test_data[,"NUM"]
new_data$TARGET <- test_data[,"TARGET"]

new_data <- test_data[c(1,132)]
write.csv(new_data, "myresult.csv", row.names=FALSE)
str(new_data)

