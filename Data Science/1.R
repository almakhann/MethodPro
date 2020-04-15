##################### 1.2
my_var1 <- 42
my_var2 <- 32.45


my_var3 <- my_var1 ^ 2 + my_var2 ^ 3
my_var3 > 2000
 
my_vector1 <- 1:67
my_vector2 <- c(-32, 15,57,32,124,-12)

my_vector1 == 0

my_vector2[my_vector2 > 0]
my_vector2[my_vector2 < 0]

my_vector1[my_vector1 > 20 & my_vector1 < 30]
?sum

my_sum = sum(my_vector1[my_vector1 > 20 & my_vector1 < 30])
my_sum

age <- c(16,18,20,22)
is_married <- c(T,F,F,T)
name = c("Olga","Nastya","Vasya","Pupkin")

data <- list(age,is_married,name)
data
data[[1]][1]

df <- data.frame(Name = name, Age =  age, Status = is_married)
df
typeof(df)


sd(age)
mean(age)
ages <- age[(mean(age) < mean(age) + sd(age)) & (mean(age) > mean(age) - sd(age))]
ages

########################## 1.3
mydata <- read.csv("evals.csv")

head(mydata, 3) #generally it is 6
tail(mydata)

View(mydata)

str(mydata)

summary(mydata)

names(mydata)


summary(mydata$score)
mydata$new_variable <- 0
mydata$new_variable


nrow(mydata)
ncol(mydata)


mydata$score[1:10]

mydata[1,1]
mydata[c(292,300),1]
mydata[nrow(mydata),]
mydata[,1]



head(mydata[,2:5])



#Subsetting
mydata$gender == "female"

summary(mydata[mydata$gender == 'female',1:3])


head(subset(mydata, gender == 'female'))

head(subset(mydata, score > 2),3)



#rbind ,cbind
mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2,mydata3)

mydata
mydata5 <- mydata[, 1:10]
mydata6 <- mydata[,11:22]
mydata7 <- cbind(mydata5, mydata6)



m1 <- subset(mtcars, cyl == 4)
m2 <- subset(mtcars, cyl != 4)

m1$mpg_4 <- m1$mpg
m2$mpg_4 <- NA
mtcars = rbind(m1,m2)
m1


mtcars

mpg_4 <- subset(mtcars, cyl == 4)$mpg
mpg_4 <- mpg_4$mpg
mpg_4


mini_mtcars <- data.frame(mtcars[c(3,7,10,12,nrow(mtcars)),])
mini_mtcars


################################### 1.4

a <- 2

if (a>0){
  print("possitive")
}else{
  print("negative")
}



ifelse(a>0,'possitive', 'negative')


mydata <- read.csv("evals.csv")

for (i in 1:nrow(mydata)){
  print(mydata$score[i])
}



mydata$quality <- rep(NA , nrow(mydata))

for (i in 1:nrow(mydata)){
  if (mydata$score[i] > 4){
    mydata$quality[i] <-  "Good"
  }else{
    mydata$quality[i] <- "Bad"}
}
View(mydata)



mydata$quality2 <- ifelse(mydata$score>4, "good", "bad")

i=0
while (i<10){
  print(i)
  i <- i + 1
}




mtcars$new_var <- 0
for (i in 1:nrow(mtcars)){
  if (mtcars$carb[i] >= 4 | mtcars$cyl[i] > 6 ){
    mtcars$new_var[i] <- 1
  }else{
    mtcars$new_var[i] <- 0 
  }
}
View(mtcars)


?AirPassengers

View(AirPassengers)

AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]]


#1.4.4
good_month <- c()
AirPassengers <- as.vector(AirPassengers)
for (i in 1:144){ 
  if (i != 144){
    if (AirPassengers[i] < AirPassengers[i+1]){
      good_month <- c(good_month, AirPassengers[i+1])
    }
  }
}

good_month


#1.4.5
moving_average <- c()
lists <- c()

AirPassengers <- as.vector(AirPassengers)
for (i in 1:144){
  if ((i+9)<=144) {
    moving_average <- c(moving_average, mean(AirPassengers[i:(i+9)]))
  }
}
lists


############################ 1.5

df<-  mtcars

df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Automatic","Manual"))

mean(df$mpg[df$cyl == 6 & df$vs == "V"])                

mean_hp_sv <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)
colnames(mean_hp_sv) = c("VS", "Mean HP")

mean_hp_sv

aggregate(hp ~ vs , df , mean)
aggregate(hp ~ vs + am , df , mean)

df

aggregate(df[,c(1,3)], by = list(df$vs , df$am), FUN = sd)
aggregate(cbind(mpg , disp)~am,df,sd)


#1.5.1
result <- mean(mtcars$qsec[mtcars$cyl!=3 & mtcars$mpg >20])
result

#1.5.2
descriptions_stat <- aggregate(cbind(hp,disp) ~ am, mtcars, sd)

library(psych)
describe(x = df)

describe(x = df[,-c(1,2)] )

descr1 <- describeBy(x = df[, -c(8,9)], group = df$vs, mat = T, digits = 1 )
descr1 
describeBy(df$qsec , group = list(df$am, df$vs) , mat = T, digits = 1)


#1.5.3
airquality

subset(airquality, Month == c(7,8,9))

#1.5
aggregate(Ozone ~ Month, subset(airquality, Month %in% c(7,8,9)), length )

#1.5.4
data <- subset(airquality, Month == 8)
describeBy(airquality$Wind, group = airquality$Month)

iris
sd(iris$Sepal.Length)


#last
?replace

my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
my_vector

replace(my_vector,is.na(my_vector), mean(my_vector, na.rm= T))


#################### 1.6 

data("mtcars")
head(mtcars)

df <- mtcars
df$am <- factor(df$am, labels = c("Autmatic", "Manual"))

hist(mtcars$mpg, breaks = 2, xlab = "MPG", ylab = "Fr")

boxplot(mpg ~ am, df)

plot(df$mpg , df$disp)


library(ggplot2) 
ggplot(df,aes(x = mpg)) + geom_histogram(fill = "white", col = 'black', binwidth = 2)
ggplot(df, aes(x = mpg, fill = am)) + geom_dotplot(fill = df$cyl, binwidth = 2)

ggplot(df, aes(x = mpg, fill = am)) + geom_density(alpha = 0.2)
ggplot(df, aes(x = mpg)) + geom_density(aes(fill= am), alpha = 0.3)

ggplot(df, aes(x = mpg, y = hp, col = vs)) + geom_boxplot(aes(fill = am))

ggplot(df , aes(x= mpg, y= hp, col = vs ,size = qsec)) + geom_point()
ggplot(airquality, aes(x = Month, y = Ozone)) + geom_boxplot()


plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp)) + geom_point()                                                   
plot1                                                           

boxplot( airquality$Ozone ~ airquality$Month )


