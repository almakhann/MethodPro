####################### 2.1

df <- read.csv("grants.csv")
str(df)

df$status = factor(df$status, labels = c("Not Funded", "Funded"))
levels(df$status)
View(df)

t1 <- table(df$status)
t1

dim(t1)

t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)
dim(t2)

prop.table(t2)
prop.table(t2,1)
prop.table(t2,2) #more clearly


t3 <- table(Year = df$years_in_uni, field = df$field, status = df$status)
t3[,2,]

dimnames(HairEyeColor)
prop.table(HairEyeColor[,"Blue","Male"],1)


#2.1.1
a <- prop.table(HairEyeColor[,'Blue', 'Male'])
a['Red']


#2.2.2
a <- HairEyeColor[ ,"Green", "Female"]
barplot(t2)


barplot(t2, legend.text = TRUE, args.legend = list(x = 'topright'))
barplot(t2, legend.text = TRUE, args.legend = list(x = 'topright'), beside = T)


mosaicplot(t2)

#ex
library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
mydata[mydata[,3] == ""]

mydata[mydata[,3] = "Female"]
subset(mydata , Sex == "Female")


obj <- ggplot(data = subset(mydata , Sex == "Female"), aes(x = Hair , y = Freq)) + geom_bar(stat="identity", 
  position = position_dodge(), aes(fill = Eye)) + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj


##BINOM TEST
binom.test(x = 10, n = 100, p = 0.7)
binom.test(t1)

##CHI-SQUARE test
chisq.test(t1)

##Fisher exact
fisher.test(t2)


a <- HairEyeColor["Brown",,"Female"]
fisher.test(a)

a <- read.table("dataset_11504_15.txt")
t.test(a$V1 ,a$V2, var.equal= TRUE)

a <- read.table("dataset_11504_15.txt")
bartlett.test(V1 ~ V2, a)
t.test(a$V1 ~ a$V2, var.equal = T)

a <- read.table("dataset_11504_16.txt")
t.test(a$V1 ,a$V2)

################################## 2.2
?iris
df  <- iris
str(df)

df1  <- subset(df, Species != "setosa")
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x =Sepal.Length ))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species ))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

#The same result in one line
by(df1$Sepal.Length, INDICES = df1$Species, shapiro.test)

bartlett.test(Sepal.Length  ~ Species, df1)


t.test(Sepal.Length  ~ Species, df1)
test1  <- t.test(Sepal.Length  ~ Species, df1)

str(test1)
test1$p.value

t.test(Sepal.Length  ~ Species, df1, var.equal = T)

t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", 
               size = 2)


?wilcox.test

test2  <- wilcox.test(Petal.Length ~ Species, df1)
pv  <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()


wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest  <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest$p.value



'X'################### 2.3
## ANOVA

library(ggplot2)

# formulae

DV ~ IV # One-way

DV ~ IV1 + IV2 # Two-way

DV ~ IV1:IV2  # Two-way interaction

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction

DV ~ IV1 * IV2  # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2

DV ~ IV1 + Error(subject/IV1) # repeated measures



# reading data

mydata <- read.csv('shops.csv')


# One-way ANOVA

boxplot(price ~ origin, data=mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()



fit <- aov(price ~ origin, data=mydata)
summary(fit)


# Two-way ANOVA

fit1 <- aov(price ~ origin + store, data=mydata)
summary(fit1)

model.tables(fit1,"means")


# Interaction

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)
anova(fit3,fit4)



# Pairwise comparisons

ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)


TukeyHSD(fit5)




# Repeated measures

mydata2 <- read.csv('therapy_data.csv')
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)


fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)


fit2 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()

fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)


fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)



'X' ################################## 2.4

my_calc  <- function(x, y){
  s  <- x + y
  return(s)
}

my_calc(x = 10, y = 15)
result  <- my_calc(10, 15)

my_calc_2  <- function(x, y){
  s  <- x + y
  d  <- x - y
  return(c(s, d))
}

my_calc_2(10, 15)


my_calc_3  <- function(x, y, z){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3  <- function(x, y, z = 10){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}



############

distr1  <- rnorm(100)
distr1[1:30]  <- NA
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T)

my_na_rm  <- function(x){
  if (is.numeric(x)){
    stat_test  <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

d1  <- rnorm(2000)
d2  <- runif(2000)

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)


######################

read_data  <- function(){
  df  <- data.frame()
  number  <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df  <- read.csv(i, stringsAsFactors = F)
    df  <- rbind(temp_df, df)
    number <<- number + 1
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

# Advanced method without for loop

read_data_advanced <- function(){
  df <- do.call(rbind, lapply(dir(pattern = "*.csv"), 
                              read.csv, stringsAsFactors = F))
  return(df)
}

df  <- data.frame(x = factor(1:5))
df1  <- data.frame(x = factor(7:8))
str(df)
str(df1)

df3  <- rbind(df, df1)
str(df3)
table(df3$x)


