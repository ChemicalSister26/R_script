library(ggplot2)

data("airquality")
df <- airquality

ggplot(df, aes(x=Month, y=Ozone, group=Month))+
  geom_boxplot()

data("mtcars")

plot1 <- ggplot(mtcars, aes(x=mpg, y=disp, col=hp)) + 
  geom_point()

data('iris')
ggplot(iris, aes(Sepal.Length, col=Species))+
  geom_histogram()
ggplot(iris, aes(Sepal.Length, fill=Species))+
  geom_histogram()

ggplot(iris, aes(Sepal.Length))+
  geom_histogram(aes(col=Species))
iris, 

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
   geom_point(aes(size = Petal.Length))
