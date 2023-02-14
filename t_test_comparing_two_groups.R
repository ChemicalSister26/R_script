iris
str(iris)
iris_short <- subset(iris, Species != 'setosa')

iris_table <- table(iris_short$Sepal.Length, iris_short$Species)
# строим гистограмму распределения
ggplot(iris_short, aes(x=Sepal.Length))+
  geom_histogram(fill='white', col='green', binwidth = 0.4)+
  facet_grid(iris_short$Species)

# строим плотность распределения
ggplot(iris_short, aes(x=Sepal.Length, fill=iris_short$Species))+
  geom_density(alpha=0.5)
# строим боксплот в поисках выбросов
ggplot(iris_short, aes(x=iris_short$Species, y=Sepal.Length))+
  geom_boxplot()

# t-test требует проверки нормальности распределения, сделаем ее Shapiro-Wilk test
shapiro.test(iris_short$Sepal.Length)
# W = 0.98054, p-value = 0.1464 нулевая гипотеза говорит о том, что исследуемое
 # распределение не отличается от нормального
# Null hypotesis of Shapiro-Wilk tells that distribution does not differ from normal 
# one, and obtained p_value tells that distribution is normal

# shapiro_wilk test for each group
shapiro.test(iris_short$Sepal.Length[iris_short$Species=='versicolor'])
#   W = 0.97784, p-value = 0.4647
shapiro.test(iris_short$Sepal.Length[iris_short$Species=='virginica'])
#   W = 0.97118, p-value = 0.2583   


# Now it's time to check homogenity of variance 

bartlett.test(iris_short$Sepal.Length~iris_short$Species)
# Bartlett's K-squared = 2.0949, df = 1, p-value = 0.1478

t_test_res <- t.test(iris_short$Sepal.Length~iris_short$Species)
# t = -5.6292, df = 94.025, p-value = 1.866e-07 
# differences are significant, we can reject the null_hypotesis that tells there
# is no difference between two compared groups
str(t_test_res)


# what if two selections are dependent from each other?
t.test(iris_short$Sepal.Length, iris_short$Petal.Width, paired = TRUE)

# null hypothesis is that those two characteristics are equal
# we reject it and adopt alternative hypothesis that mean difference in not 0

# now let's visualize obtaintd results from test

ggplot(iris_short, aes(x=Species, y=Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom='errorbar', width=0.1)+
  stat_summary(fun=mean, geom='point', size=4)


# non parametric test Wilcoxon test Mann-Whitney
wilcox.test(Sepal.Length ~ Species, iris_short)
