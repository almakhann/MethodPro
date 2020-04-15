'X'############### 3.1

#
#Correlation and simple regression
#
df <- mtcars

core <- cor.test(df$mpg,df$hp)
str(core)

cor.test(~ mpg + disp, df)

plot(df$mpg, df$hp)
library(ggplot2)
ggplot(df, aes(mpg, hp, col = factor(cyl))) + geom_point(size =5)

df
df_numeric <- df[,c(1,3:7)]
df_numeric

pairs(df_numeric)
cor(df_numeric)

library(psych)
fit <- corr.test(df_numeric)
fit$r  #Cor
fit$p   #p value

#EX
corr.calc <- function(x){
  df <- x
  a <- corr.test(df)
  b <-  a$r[2]
  b[2] <- a$p[2]
  return (b)
}
corr.calc(df[,c(1:2)])


df_numeric <- df[,c(1,3:7)]
#EX
filtered.cor <- function(x){
  library(psych)
  data <- x
  nums <- unlist(lapply(data, is.numeric))  
  cor <- corr.test(data[,nums])$r
  diag(cor) <- NA 
  max <- cor[which.max(abs(cor))]
  return (max)
}


#EX
smart_cor <- function(x){
  data <- x
  a <- shapiro.test(data[1][,1])$p.value
  b <- shapiro.test(data[2][,1])$p.value
  if (a < 0.05 | b < 0.05){
    cor <- cor.test(data[1][,1],data[2][,1],method = "spearman")$estimate
    return (cor)
  }
  else{
    cor <- cor.test(data[1][,1],data[2][,1],method = "pearson")$estimate
    return(cor)
  }
}
smart_cor(a)

data <- mtcars
a <- data[,1:2]

?shapiro.test

shapiro.test(a[1][,1])

cor.test(a[1][,1],a[2][,1],method = "pearson")$estimate
cor.test(a[1][,1],a[2][,1],method = "spearman")$estimate


##Sample Linear regression
df <- mtcars
df_numeric <- df[,c(1,3:7)] 

?lm
fit <- lm(mpg ~ hp , df)
fit

summary(fit)

ggplot(df,aes(hp,mpg, col = factor(am))) + geom_point() + geom_smooth(method = "lm") +
  facet_grid(.~vs) #with color

ggplot(df,aes(hp,mpg)) + geom_point() + geom_smooth(method = "lm") +
  facet_grid(.~vs)

ggplot(df,aes(hp,mpg)) +  geom_smooth(method = "lm",se = F) + facet_grid(.~vs) #withous doveritelniy interval

fitted_value <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)
View(fitted_value)

#New Data frame and prediction
new_hp <- data.frame(hp =c(100,150,200,400))
predict(fit, new_hp)
new_hp$mpg <- predict(fit, new_hp)
View(new_hp)

my_df <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

ggplot(my_df, aes(cyl, mpg)) + geom_point() + theme(axis.text = element_text(size = 25),
                                    axis.title = element_text(size = 25 ,face = 'bold'))

aggregate(mpg ~ cyl,my_df, FUN = mean)


##EX
data <- read.table("dataset_11508_12.txt")
fit <- lm(data[,1]~data[,2],data)
fit

##EX
qwe <- diamonds
q <- subset(qwe,cut == 'Ideal')
data <- subset(q, carat == 0.46)
fit_coef <- lm(price~depth,data)$coefficients
fit_coef


##EX
regr.calc <- function(x){
  data <- x
  a <- cor.test(data[1][,1],data[2][,1], method = "pearson")$p.value

  if (a < 0.05){
    fit <- lm(data[1][,1]~data[2][,1], data) 
    data$fit <- predict(fit, data)
    return (data)
  }
  else{
    return("There is no sense in prediction")
  }
}
regr.calc(mtcars[1:2])


##Ex
data <- iris
data
ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species)) + geom_point() +
  geom_smooth(method = "lm")


'X'############### 3.2


#
#Multiple Linear Regression
#

?swiss
swiss <- swiss

str(swiss)

hist(swiss$Fertility, col = "Red")

#numeric predictio
fit <- lm(Fertility ~ Catholic + Examination, swiss)
summary(fit)

fit2 <- lm(Fertility ~ Catholic * Examination, swiss)
summary(fit2)

confint(fit2)


##EX
fill_na <- function(x){
  test_data <- x
  fit <- lm(test_data[,3]~test_data[,2]+test_data[,1], test_data)
  test_data$y_full <- test_data$y
  test_data$y_full <- ifelse(is.na(test_data$y_full),predict(fit, test_data), test_data$y)
  return (test_data)
}

fill_na(test_data)
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
test_data


##EX !!!!!!!!!!!!!!!!!!
data <- mtcars
df <- mtcars[c(6,1,3,5,4)]
df

fit <- lm(mpg ~ wt*factor(am),data)
summary(fit)

(data)
labels(data$m)

fit1 <- 
  
  
  data <- attitude
data
fit <- lm(rating ~ critical * complaints,data)
a <- summary(fit)
a
a$adj.r.squared
model <- summary(lm(wt ~ disp,df))$adj.r.squared
model


#catecorial Prediction
hist(swiss$Fertility, col= "Red")
swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)


##Plots
ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


#f
fit5 = lm(Fertility ~ religious*Infant.Mortality*Examination, swiss)
summary(fit5)



#exxx
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
fit6 <- lm(mpg~wt*am, mtcars)
summary(fit6)


#Ex
ggplot(mtcars,aes(wt,mpg, col= am)) + geom_smooth(method = "lm")


#model Cmomparision
rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

#Model Selection
optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)




#EX
model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)

scope = list(lower = model_null, upper = model_full)

a <- step(model_full,scope, direction = 'backward')

#EX
anova(a, model_full)


#ex
ex_data <- LifeCycleSavings
ex_data
fir <- lm(sr ~ .,ex_data)
fir


'X'############### 3.3-5

#
#Regression diagnostic
#

data(swiss)
swiss

#Relations
pairs(swiss)

ggplot(swiss , aes(Fertility, Examination)) +geom_point()


#Outliers
ggplot(swiss , aes(Fertility, Examination)) +geom_point() +geom_smooth(method = "lm")


#Normality of variable distribution
ggplot(swiss, aes(log(Education))) + geom_histogram()


##EX
?scale
beta.coef <- function(x){
  a <- x
  b <- scale(a[,1])
  d <- scale(a[,2])
  fill <- lm(b[,1] ~ d[,1],a)$coefficients
  return(fill)
}
data(mtcars)
a <- mtcars[,c(1,3)]
beta.coef(a)


##EX
normality.test  <- function(x){
  data <- x
  a <- c()
  for (i in 1:length(data)) {
    a[i] <- c(shapiro.test(data[,i])$p.value)
  }
  names(a) <- names(data)
  return(a)
}
normality.test(x)

shapiro.test(mtcars[,1])$p.value
x <- mtcars[,c(1:3)]
x[2]


#Linearity
ggplot(swiss, aes(Education,Examination)) + geom_point() + geom_smooth()

lm1 <- lm( Education ~ Examination,swiss)
summary(lm1)

swiss$Examination_squared <- (swiss$Examination^2)
swiss

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)

anova(lm1,lm2)

lm3 <- lm(log(Education) ~ log(Examination) , swiss)
summary(lm3)
cor.test(swiss$Examination,swiss$Education)

swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) + 
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd=1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd=1)
swiss


ggplot(swiss, aes(lm1_fitted, lm1_resid)) + geom_point() + geom_line(y = 0, col= "Red",lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_line(y=0, col = 'red', lwd = 1)


##independence of errors

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()


# Homoscedasticity

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)




#ex
df = read.csv("homosc.csv")
df

library(gvlma)
?gvlma
x <- gvlma(DV~IV,df)
a <- summary(x)
?glm

# Errors Normally distributed

ggplot(swiss, aes(x = lm1_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)


ggplot(swiss, aes(x = lm2_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)

##EX
data("mtcars")
resid.norm  <- function(fit){
  data <- fit
  test <- shapiro.test(data$residuals)
  fit.residuals <- data.frame(data$residuals)
  if (test$p.value < 0.05 ){
    return (ggplot(data, aes(fit.residuals )) + 
              geom_histogram(fill = "Red"))
  }
  else{
    return(ggplot(data, aes(fit.residuals )) + 
             geom_histogram(fill = "Green"))
  }
}

fit <- lm(mpg ~ disp, mtcars)
fit <- lm(mpg ~ wt, mtcars)
resid.norm(fit)


#EXX
high.corr <- function(x){
  df <- x
  cor <- corr.test(df)$r
  diag(cor) <- NA
  value <- which.max(abs(cor))
  k <- arrayInd(value, dim(cor))
  return (c(rownames(cor)[k[,2]],colnames(cor)[k[,1]]))
  
}
high.corr(test_data)


df <- swiss
df


test_data <- as.data.frame(list(V1 = c(0.4, 1.1, 1.6, 1, 2.1), 
                                V2 = c(0.7, -0.7, 1.7, -1.3, -0.1), 
                                V3 = c(0.3, -0.6, -1, 2.3, -0.6), 
                                V4 = c(0, 0.1, -1.6, -0.9, 2), 
                                V5 = c(0.6, -1.2, 2, 0.3, -0.6), 
                                V6 = c(0.3, -1.5, 0.4, 0.5, -2.2), 
                                V7 = c(-1.4, -1.3, 0.3, 0.4, 1.2), 
                                V8 = c(-0.4, -1.3, -0.2, -0.4, -0.3), 
                                V9 = c(0, 1, 0, -0.2, -0.5), 
                                V10 = c(1.8, 0.3, -1, 0, -1.2), 
                                V11 = c(-0.3, 0.6, 1, -2.3, 0.6)))
test_data
corr.test(test_data)$r


'X'############### 3.6


library(ggplot2)

my_df <- read.csv("train.csv", sep=";")
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 5)+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))


fit  <- glm(hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)

exp(fit$coefficients)

head(predict(object = fit))

head(predict(object = fit, type = "response"))

my_df$prob  <- predict(object = fit, type = "response")


data(mtcars)
mtcars
##EX
fit <- glm(am~disp+vs+mpg,mtcars,family = "binomial")$coefficient
fit

##Ex
ggplot(data = ToothGrowth, aes(supp,len)) + geom_boxplot(aes(fill = factor(dose)))

library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)



perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.225, lwd = 2)


my_df$pred_resp  <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct  <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)


ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

mean(my_df$correct)


test_df  <- read.csv("test.csv", sep = ";")
test_df$hon  <- NA

test_df$hon  <- predict(fit, newdata = test_df, type = "response")
View(test_df)


data <- read.csv("https://stepic.org/media/attachments/lesson/11478/data.csv")
data



