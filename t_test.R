data('iris')
iris <- iris
# structure
str(iris)

# let's choose subset

df <-  subset(iris, Species!='setosa')
str(df)
table(df$Species)

# before t-test we should check if the distribution is normal and 
# dispersion distributed homogenously

ggplot(df, aes(x=Sepal.Length))+
  geom_histogram(fill='green', binwidth=0.4)+
facet_grid(df$Species)

ggplot(df, aes(y=df$Sepal.Length, x=df$Species))+
  geom_boxplot(fill='green')


# shapiro-wilk test p-value это проверка нулевой гипотезы о том, что распреедление
# НЕ ОТЛИЧАЕТСЯ от нормального
sh_t <- shapiro.test(df$Sepal.Length)
sh_t
#p-value = 0.1464
# проверим по каждой отдельной группе ирисов.

shapiro.test((df$Sepal.Length[df$Species=='versicolor']))
# p-value = 0.4647
shapiro.test((df$Sepal.Length[df$Species=='virginica']))
# p-value = 0.2583

# теперь проверим гомогенность дисперсий
bartlett.test(Sepal.Length ~ Species, df)
# p-value = 0.1478 - гомогенность дисперсий соблюдается

test1 <- t.test(Sepal.Length ~ Species, df)
# p-value = 1.866e-07 такое значение позволяет нам отклонить 
# нулевую гипотезу о равенстве средних
# 95 percent confidence interval: -0.8819731 -0.4220269 не включает
# в себя 0, то есть разница между средними никогда не равна 0
# и нулевая гипотеза отклоняется

str(test1)

# how to visualize t.test results?
ggplot(df, aes(x=Species, y=Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom='errorbar')+
  stat_summary(fun.y = mean, geom='point')
