data('iris')
iris
summary(iris)
view(iris)
#the dataset is composed by 5 variables, 4 numerical and one categorical with 3 levels(setosa , versicolor, virginica)
#there are 150 samples, no NA values

help(iris)
is.na(iris)

lm(iris)
library(rpart)
help.start()


rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris, method="class")
rpart(Species~.,data=iris, method="class")

output=rpart(Species~.,data=iris, minsplit=1 , cp=0)
output

plot(output)
text(output)


data('mtcars')
mtcars
help(mtcars)
output1=rpart(mpg~.,data=mtcars)
output1
plot(output1)
text(output1)
output2=rpart(mpg~.,data=mtcars, minsplit=1 , cp=0)
output2
