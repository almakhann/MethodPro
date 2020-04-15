Accuracy = (TP+TN)/(TP+TN+FP+FN)
Precision = TP/(TP+FP)
Recall = Sensitivity = TP/(TP+FN)
F-measure = 2Precisionrecall/(precision+recall)
ROC curve(Area under the curve) = Precision*Recall
Error rate = 1 - Accuracy


library('ggplot2')
library('dplyr')
data('diamonds')
head(diamonds)


general <- diamonds
sample <- sample_frac(diamonds,0.2)
head(general)

#Visualization  
#qplot - quick plot
#qplot(x,y,)
?qplot

histogram(sample)

qplot(x= carat,y=price,
      color = cut,
      data = sample)
boxplot(sample$carat)

##EXAMPLE1
data('mtcars')
head(mtcars)
datas <- mtcars

datas$cyl = factor(datas$cyl)
qplot(x = datas$mpg, y = datas$disp,
      color = datas$cyl)

##EXAMPLE2
qplot(x = datas$mpg, y = datas$disp,
      color = datas$cyl)
qplot(x = datas$mpg , fill = datas$cyl, binwidth = 2)

str(datas$cyl)

##EXAMPLE3
datas$carb = factor(datas$carb)
ggplot(datas, aes(mpg,disp)) + geom_point(aes(color = carb), size = 2.5,
                                          alpha = 0.8)

##EXAMPLE4
ggplot(datas, aes(mpg,disp)) + geom_point(aes(color = cyl, size = qsec))


##Example5
ggplot(datas, aes(mpg)) +  geom_density(aes(fill=cyl), alpha = 0.5)

##EX6
ggplot(datas, aes(mpg,disp, group = cyl)) + geom_boxplot(varwidth = T)

##EX7
ggplot(datas$mpg, aes(manufacturer)) + geom_bar(aes(fill = class), width = 0.5) + 
  theme(axis.text.x = element_text(angle = 63))

