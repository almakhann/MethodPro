train <- read.csv("train.csv")
View(train)

sum(is.na(train))
colnames(train)[apply(train,2,anyNA)]

##Removing NA with for
mean <- mean(na.exclude(train$Age))
for (i in 1:length(train$Age)){
    if (is.na(train$Age[i]) == TRUE){
        train$Age[i] = mean
    }
}
str(train)

#Removing with apply
train %>% mutate(ifelse(is.na(train$age), mean ,train$age))



sum(is.na(train$Age))

a <- train[,-c(1,4,9,11)]
str(a)

View(a)

pairs(a)


library(PerformanceAnalytics)
chart.Correlation(a[,-c(3,8)])

library(psych)
corr.test(a[,-c(3,8)])
str(a)

library(dplyr)
a <- a %>%  mutate_if(is.character,as.factor)
a <- a %>%  mutate_if(is.integer,as.numeric)


library(rpart)
##lm
fit1 <- lm(Survived~.,a)
summary(fit1)

fit2 <- glm(Survived~(Pclass+Sex+Age+SibSp)^2, a, family = "binomial")
summary(fit2)

fit3 <- rpart(Survived~.,a,method = "class")
summary(fit3)

prop.table(table(train$Survived))

#prediction
p1 <- predict(fit2,a)
p1 <- ifelse(p1 >= 0.5,1,0)


##Test Data
test <- read.csv("test.csv")
View(test)

mean <- mean(na.exclude(test$Age))
for (i in 1:length(test$Age)){
  if (is.na(test$Age[i]) == TRUE){
    test$Age[i] = mean
  }
}


test$Survived <-  predict(fit2,test)
str(test)



new_data <- data.frame(test[,"PassengerId"])
new_data$Survived <- test[,"Survived"]
View(new_data)
new_data$Survived <- ifelse(new_data$Survived >= 0.5,1,0)
sum(is.na(new_data$Survived))


write.csv(new_data, "myresult.csv", row.names=FALSE)
rm(new_data)





x <- c(4,5,2,3,1)
y <- c(2,1,4,3,5)

cor(x,y)

















