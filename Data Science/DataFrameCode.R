library(ggplot2)
library(dplyr)
library(mlmRev)
library(lme4)
library(missForest)
install.packages("missForest")
# load data
df <- read.csv("train.csv")
# data eng

cols <- sapply(df, function(x){sum(is.na(x)/nrow(df))})<=0.4
df <- df[,cols]
df <- df[df$SalePrice<=mean(df$SalePrice)+sd(df$SalePrice),]
boxplot(df$SalePrice)
shapiro.test(df$SalePrice)
# extrapolation
m <- missForest(df)
df <- m$ximp
m$OOBerror
sum(is.na(df))

# ulovki

fit <- lm(SalePrice~.,df)
summary(fit)

check <- function(x,data){
  lm <- lm((x$residuals)^2~.,data)
  summary(lm)
}
check(fit,df)

fit2 <- lm(log(SalePrice)~.,df)
check(fit2,df)
plot(fit2)

#

# one hot encoding
library(dummies)
#      - get factor columns
library(dplyr)
df <- df %>%  mutate_if(is.character,as.factor)
df <- df %>%  mutate_if(is.integer,as.numeric)


df_fac <- df[,sapply(df, function(x){is.factor(x)})]
df_num <- df[,sapply(df, function(x){is.numeric(x)})]

head(df_fac)
df_dummy <- dummy.data.frame(data = df_fac)

# log NP
# nelzya
sapply(df_num,function(x){log(x)})
# hudshee
sapply(df_num,function(x){log(x+1)})
# bolee menee
sapply(df_num,function(x){log(abs(x)+1)}) # norm tak

df_num <- sapply(df_num,function(x){log(abs(x)+1)})

df_total <- cbind(df_num,df_fac)

fit_3 <- lm(SalePrice~.,df_total)
summary(fit_3)
check(fit_3,df_total)
plot(fit_3)
#step(fit_3,direction = "backward")


# xgbfi
# mean encoding

data("diamonds")

# by each cut get mean of price
diamonds %>% group_by(cut,color) %>% summarise(mean(price),
                                               sd(price),
                                               max(carat))


sample_n()
sample_frac(diamonds,.3)
df_total$Id <- NULL
library(caret)
index <- createDataPartition(df$SalePrice,p=0.7,list=FALSE)
train <- df[index,]
test <- df[-index,]
library(randomForest)
# ntree - kol-vo derevyev
# do.trace - chtoby videt process
# type - , "regression" , "classification"
rf <- randomForest(SalePrice~.,train,ntree=500,do.trace=TRUE,
                   type="regression")

fit4 <- lm(SalePrice~.,df_total)
summary(fit4)
# varImpPlot - Feature Importance 
varImpPlot(rf)


pred_rf  <- predict(rf,test)
pred_lm <- predict(fit4,test)


RMSE(test$SalePrice,pred_rf)
RMSE(test$SalePrice,pred_lm)





### 
rf <- randomForest(SalePrice~.,train,ntree=500,do.trace=TRUE,
                   type="regression")

fit4 <- lm(SalePrice~.,df_total)

# varImpPlot - Feature Importance 
varImpPlot(rf)


pred_rf  <- predict(rf,test)
pred_lm <- predict(fit4,test)


RMSE(test$SalePrice,pred_rf)
RMSE(test$SalePrice,pred_lm)
Message Input


Message #datascience