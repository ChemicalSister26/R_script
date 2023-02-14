res_cor <-  cor.test(x=mtcars$mpg, y=mtcars$hp, method = 'pearson')


str(res_cor)
# the same as above equation
cor.test(~ mpg+hp, mtcars)

# we know that correlation coeff PEARSON MAY NOT BE PRECIOUS 
# in estimation of correlation between two variables, so let's plot a fugure
plot(x=mtcars$mpg, y=mtcars$hp)

ggplot(mtcars, aes(x=mpg, y=hp, col=factor(cyl)))+
  geom_point()
 # let's choose only numeric values from the whole dataset
mtcars_num <- mtcars[, c(1,3:7)]

pairs(mtcars_num)

# Напишите функцию corr.calc, которая на вход получает
# data.frame с двумя количественными переменными, рассчитывает
# коэффициент корреляции Пирсона и возвращает вектор из двух 
# значений: коэффициент корреляции и p - уровень значимости.

corr.calc <- function(x){
  colnames(x) <- c("v1", 'v2')
  res <- cor.test(~ v1+v2, x)
  return(c(res$estimate, res$p.value))
}

# Напишите функцию filtered.cor которая на вход получает data.frame
# с  произвольным количеством переменных (как количественными, 
# так и любых других типов), рассчитывает коэффициенты корреляции
# Пирсона между всеми парами количественных переменных и возвращает 
# наибольшее по модулю значение коэффициента корреляции. 
#(То есть функция может вернуть -0.9, если это наибольшая по модулю  корреляция).
# Гарантируется наличие в data.frame хотя бы двух количественных переменных.
# Обратите внимание: при проверке вашей функции на вход будут 
# подаваться данные с различными именами колонок. Ваша функция должна 
# корректно работать независимо от имен переменных. Перед тем, как сдавать решение,
# убедитесь, что ваша функция работает корректно на разных данных, 
# с разными именами колонок. 


filtered.cor <- function(x){
  nums <- unlist(lapply(x, is.numeric), use.names = FALSE) # takes only numeric cols
  x_num <- x[ , nums]
  cor_table <- corr.test(x_num)
  cor_matrix <- cor_table$r
  diag(cor_matrix) <- 0
  res1 <-  c(abs(max(cor_matrix)), abs(min(cor_matrix)))
  if (max(res1) == max(cor_matrix)){
    return(max(cor_matrix))
  }
  else {
    return(min(cor_matrix))
  }
}

# Напишите функцию smart_cor, которая получает на вход dataframe
# с двумя количественными переменными. Проверьте с помощью теста Шапиро-Уилка, 
# что данные в обеих переменных принадлежат нормальному распределению.
# Если хотя бы в одном векторе распределение переменной отличается от
# нормального (p - value меньше 0.05), то функция должна возвращать коэффициент
# корреляции Спирмена. (Числовой вектор из одного элемента).
# Если в обоих векторах распределение переменных от нормального значимо 
# не отличается, то функция должна возвращать коэффициент корреляции Пирсона.


smart_cor <- function(x){
  colnames(x) <- c("v1", 'v2')
  test1 <- shapiro.test(x$v1)
  test2 <- shapiro.test(x$v2)
  if (test1$p.value < 0.05 | test2$p.value < 0.05) {
    return(cor.test(x$v1, x$v2, method = 'spearman')$estimate)
  } else {
    return(cor.test(x$v1, x$v2, method = 'pearson')$estimate)
  }
}

# now, let's create linear regression

mtcars_num
l_m <- lm(mpg ~ hp, mtcars_num)
summary(l_m)


ggplot(mtcars, aes(hp, mpg))+
  geom_point()+
  geom_smooth()

ggplot(mtcars, aes(hp, mpg))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(mtcars, aes(hp, mpg))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_grid(.~cyl)

# our model was created for prediction, so we can get predicted values
# from the model

df_y_pred <- data.frame(mtcars$mpg, l_m$fitted.values)

# this dataframe contains existing in main dataframe mtcars values
# and approprite to them values taken from predicted with linear regression
# help. But let's obtain predicted values for some new numbers

new_hp <- data.frame(hp = c(120, 133, 250, 303))
predict(l_m, new_hp)
new_hp$mpg <- predict(l_m, new_hp)


# what should we do if our independent variable is not quantitative but
# is a nominative one?
data('mtcars')
mtcars$cyl <- factor(mtcars$cyl, labels = c('four', 'six', 'eight'))
l_m_nom <- lm(mpg ~ cyl, mtcars)
summary(l_m_nom)

#let's calculate mean in our three groups - four cyl, siscyl and eight cyl

aggregate(mpg ~ cyl, mtcars, mean)


ggplot(mtcars, aes(x=mtcars$cyl, y=mtcars$mpg))+
  geom_point()+
  geom_smooth(method ='lm')

df_task1 <-  read.table('dataset_11508_12.txt')
l_m_task1 <- lm(data = df_task1, V1 ~ V2)
summary(l_m_task1)

# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. 
# Только для бриллиантов класса Ideal (переменная cut) c числом карат
# равным 0.46 (переменная carat) постройте линейную регрессию, где в качестве 
# зависимой переменной выступает price, в качестве предиктора - переменная 
# depth. Сохраните коэффициенты регрессии в переменную fit_coef.

data("diamonds")
diamonds
df_task2 <- subset(diamonds, cut == 'Ideal' & carat == 0.46)
l_m_task2 <- lm(price~depth, df_task2)
summary(l_m_task2)
fit_coef <- l_m_task2$coefficients

# Напишите функцию regr.calc, которая на вход получает dataframe 
# c двумя переменными. Если две переменные значимо коррелируют 
# (p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05),
# то функция строит регрессионную модель, где первая переменная - зависимая, 
# вторая - независимая. Затем создает в dataframe новую переменную с назанием fit, 
# где сохраняет предсказанные моделью значения зависимой переменной. 
# В результате функция должна возвращать исходный dataframe с добавленной 
# новой переменной fit. Если две переменные значимо не коррелируют, 
# то функция возвращает строчку "There is no sense in prediction"


regr.calc <- function(x){
  if (cor.test(x[[1]], x[[2]])$p.value < 0.05) {
  res <- lm(x[[1]] ~ x[[2]])
  x$fit <- res$fitted.values
  return(x)
} else {
  return("There is no sense in prediction")
}
}

regr.calc(iris[,1:2])


iris
my_plot <-  ggplot(iris, aes(x=Sepal.Width, y=Petal.Width, col=Species))+
  geom_point()+
  geom_smooth(method='lm')