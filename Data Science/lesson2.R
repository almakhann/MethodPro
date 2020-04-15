data("mtcars")

head(mtcars)

mtcars$new <- ifelse(mtcars$mpg > mean(mtcars$mpg), 1, 0)
View(mtcars)

mtcars$mpg <- NULL


#formula to split data 80/20
#install packet caret
library(caret)

index <- createDataPartition(mtcars$new, p = 0.8, list = FALSE)

train <- mtcars[index,]
test <- mtcars[-index,]

library(rpart)

#rpart(????~a+s+c+d+w,data =)
decision_tree <- rpart(new~.,train)

#predict(model,test)
predicted <- predict(decision_tree,test)
library(ROSE)

#Roc curve - ?????????????? ???????????????????? ?????? ???? ????????????
roc.curve(test$new,predicted)
View(mtcars)


data("diamonds")
head(diamonds)
View(diamonds)
diamonds$price <- NULL

diamonds$new <- ifelse(diamonds$carat > 0.21, 10, 15)

index2 <- createDataPartition(mtcars$new, p = 0.8 , list = FALSE)
train2 <- diamonds[index2,]
test2 <- diamonds[-index2,]

decision_tree <- rpart(new~.,train2)
predicted <- predict(decision_tree,test2)

roc.curve(test2$new,predicted)

