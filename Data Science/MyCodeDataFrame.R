train <- read.csv("train.csv")
View(train)



nums <- unlist(lapply(train, is.numeric))  
t <- train[nums]

#For gettin NA
str(t)
sapply(t,function(x){sum(is.na(x))})
is.na(t) 

#change NA to MEAN every row
t[] <- lapply(t, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})


for(i in 1:ncol(t)){
  t[is.na(t[,i]), i] <- mean(t[,i], na.rm = TRUE)
}

##Getting mean
mean <- sapply(t,FUN=mean)


fit1 <- lm(SalePrice ~ ., t)
summary(fit1)

fit2 <- lm(SalePrice ~ MSSubClass+LotArea+OverallQual+OverallCond+YearBuilt+
             MasVnrArea+BsmtFinSF1+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BsmtFullBath+
             BedroomAbvGr+TotRmsAbvGrd+GarageCars+WoodDeckSF+ScreenPorch+PoolArea
           ,t)
summary(fit2)

fit3 <- lm(SalePrice ~ MSSubClass+LotArea+OverallQual+OverallCond+YearBuilt+
             MasVnrArea+X1stFlrSF+X2ndFlrSF+BsmtFullBath+
             BedroomAbvGr+TotRmsAbvGrd+GarageCars,t)
summary(fit3)

fit4 <- lm(SalePrice~(MSSubClass+LotArea+OverallQual+OverallCond+YearBuilt+MasVnrArea+
          X1stFlrSF+X2ndFlrSF+FullBath+BedroomAbvGr+TotRmsAbvGrd+GarageCars
          +ScreenPorch)^3,t)
summary(fit4)

fit5 <- lm(SalePrice~(MSSubClass+LotArea+OverallQual+OverallCond+YearBuilt+MasVnrArea+
                        X1stFlrSF+X2ndFlrSF+FullBath+BedroomAbvGr+TotRmsAbvGrd+GarageCars
                      +ScreenPorch)^2,t)
summary(fit5)

fit_B = lm(SalePrice~(MSSubClass+LotArea+OverallQual+OverallCond+MasVnrArea+
                       MSSubClass:YearBuilt+LotArea:MasVnrArea+LotArea:X1stFlrSF+
                       LotArea:X2ndFlrSF+LotArea:FullBath+
                       LotArea:BedroomAbvGr+LotArea:TotRmsAbvGrd)^2,t)

fit_last <- lm(SalePrice~(MSSubClass*LotArea + LotArea*OverallQual + 
            OverallCond*LotArea + YearBuilt )^2, t)
summary(fit_last)

View(t)



##TEST DATA
test <- read.csv("test.csv")

nums <- unlist(lapply(test, is.numeric))  
new_test <- test[nums]

sum(is.na(new_test))

#change NA to MEAN every row
new_test[] <- lapply(new_test, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})
sum(is.na(test))

test$SalePrice <- predict(fit4,)
View(test)


test <- test %>%  mutate_if(is.character,as.factor)
test <- test %>%  mutate_if(is.integer,as.numeric)

rm(new_data)
##Saving Data
new_data <- data.frame(new_test[,"Id"])
new_data$SalePrice <- new_test[,"SalePrice"]
View(new_data)

write.csv(new_data, "myresult.csv", row.names=FALSE)
rm(new_data)

new_data[is.na(new_data)]
