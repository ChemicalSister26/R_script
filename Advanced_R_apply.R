diamonds
# let's find minimum for x, y and z - sizes of diamonds - in each column

min_size <- numeric(nrow(diamonds))

# margin == axis
min_size <- apply(diamonds[, 8:10], MARGIN=1, FUN=min)

# for example we will create a matrix rnorn - normal distribution
matrix <- matrix(rnorm(30), nrow=5)
res <- apply(matrix, MARGIN = 1, FUN = sd)
my_range <- apply(matrix, 1, range)



# function returns values of vector x if they are outliers and lie beyond
# 2 Standard Deviations from mean of the vector
outliers <- function(x) {
  outliers <- x[abs(x-mean(x)) > 2*sd(x)]
  if (length(outliers) > 0) {
    return(outliers)
  } else {
    return('There are no outliers')
  }
}

# we will take subset from iris with numeric values
iris_num <- iris[, 1:4]
str(iris_num)
iris_outliers <- apply(iris_num, 2, outliers)
str(iris_outliers)


airquality
apply(airquality, 2, mean)

mean(airquality$Ozone, na.rm=T)
apply(airquality, 2, mean, na.rm=T)
# it will be better to use 
colMeans()
colSums()
rowMeans()
rowSums()
# instead of apply(..., mean)

set.seed(42)
m <- as.data.frame(matrix(rnorm(30), nrow=5))

# analogue of lambda in Python
f <- function(x) x[x<0]
f(1)
f(2:8)
apply(m, 2, function(x) x[x<0 & !is.na(x)])
vector <- as.data.frame(c(NA, 0, -10))
apply(vector, 2, function(x) x[x<0 & !is.na(x)])

# Напишите функцию get_negative_values, которая получает на вход 
# dataframe произвольного размера. Функция должна для каждой переменной
# в данных проверять, есть ли в ней отрицательные значения. 
# Если в переменной отрицательных значений нет, то эта переменная нас не интересует,
# для всех переменных, в которых есть отрицательные значения мы сохраним их в
# виде списка или матрицы, если число элементов будет одинаковым в 
# каждой переменной (смотри пример работы функции).

test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))

get_negative_values <- function(x){
 res <- apply(test_data[, 1:length(test_data)], 2, function(x) x[x<0 & !is.na(x)])
 res[sapply(res,length) > 0]
}

get_negative_values(test_data)


# we can apply ANOVA with apply function to several numeric columns at once
str(iris)
aov_stat <- apply(iris[, 1:4], 2, function(x) aov(x ~ iris$Species))
aov_stat$Sepal.Length
norm_dist <- apply(iris[, 1:4], 2, 
                   function(x) shapiro.test(x)$p.value)


# Напишите функцию na_rm которая заменяет все пропущенные значения в
# столбцах dataframe на соответствующее среднее значение. То есть все NA 
# в первом столбце заменяются на среднее значение первого столбца 
# (рассчитанного без учета NA). Все NA второго столбца заменяются на 
# среднее значение второго столбца и т.д. Функция na_rm на вход получает
# dataframe произвольной размерности, состоящий из количественных переменных. 
# Функция должна возвращать  dataframe с замененными NA.
# Ни порядок столбцов, ни порядок строк в dataframe изменять не нужно.
# Вы можете создавать вспомогательные функции для решения этой задачи. 
# Напоминаю, что для проверки является ли наблюдение NA нужно использовать 
# функцию is.na()
test_data_rm <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 
                                       8, 9, 11, 11, 10, 12, 9),
                                V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 
                                       10, 10, 11, 10, 10, 10),
                                V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 
                                       12, 8, 8, 10, 10, 8), 
                                V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12,
                                       10, 7, 10, 13, 10, 9)))
na_rm  <- function(x){
  as.data.frame(apply(x, 2, function(x) ifelse(is.na(x), mean(x, na.rm = T), x)))
}

# lapply ----- lapply(list, function)

list_lapply <- list(x=rnorm(30), y=rnorm(10))
lapply(list_lapply, mean)
#$x
#[1] -0.1219085

#$y
#[1] 0.5390768

# sapply ----- simplifies returned list to vector or matrix
sapply(list_lapply, mean)
#x          y 
#-0.1219085  0.5390768 


# Напишите функцию positive_sum, которая получает на вход 
# dataframe с произвольным количеством числовых переменных.
# Основная задача функции - найти сумму положительных значений 
# в каждой переменной и сохранить их в список. 
# Обратите внимание, если в переменной нет положительных значений, 
# или все значения NA, возвращаем для это переменной 0. 

d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))

positive_sum <-  function(test_data){
  res1 <- apply(test_data, 2, function(x) x[x>0 & !is.na(x)])
  lapply(res1, sum)
}
positive_sum(d) 

# Предположим у нас есть dataframe с двумя переменными name - название гена, 
# expression - уровень экспрессии. Например:
#    > my_data
# name expression
# 1 p1@HPS1       120
# 2 p2@HPS2       89
# 3 p@GOT1        45
# Обратите внимание, что само название гена спрятано внутри строки
# и указано после символа @. Напишите функцию my_names, которая 
# получает на вход  датафрейм и вектор с именами тех генов, 
# для которых мы хотим отобрать наблюдения уровня экспрессии. 
# Допустим, мы хотим отобрать наблюдения только для генов 'HPS1' и 'GOT1',
# тогда результат работы функции будет следующий:
#  > names =c('HPS1', 'GOT1')
# > my_names(my_data, names)
# name expression
# 1 p1@HPS1        120
# 3  p@GOT1         45
# Таким образом в процессе проверки на вход вашей функции будет 
# подаваться два аргумента:
# 1. Датафрейм, c произвольным количеством строк, где имена генов сохранены в 
# переменной names (фактор)  в формате ****@name , а уровень экспрессии в 
# переменной expression.
# 2. Вектор с именами генов, для которых мы хотим отобрать наблюдения. 
# Гарантируется, что имена указанные в векторе есть в данных.
# Функция возвращает датафрейм с наблюдениями только для указанных генов.

test_data <- as.data.frame(list(name = c("p4@HPS1", 
                                         "p7@HPS2", "p4@HPS3", "p7@HPS4", 
                                         "p7@HPS5", "p9@HPS6", "p11@HPS7", 
                                         "p10@HPS8", "p15@HPS9"), 
                                expression = c(118.84, 90.04, 106.6, 
                                               104.99, 93.2, 66.84, 90.02,
                                               108.03, 111.83)))
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")


my_names <- function (dataset, names){
  true_vec <- sapply(test_data['name'], function(x) grepl(paste(names, collapse = "|"), x))
  test_data[true_vec, ]
}

my_names(test_data, names)


# Рассмотрим следующую ситуацию: у нас есть dataframe с произвольным 
# числом количественных переменных. Мы хотим построить линейную регрессию 
# для предсказания значений зависимой переменной, однако, в качестве 
# предикторов мы хотим использовать только те переменные, распределение 
# которых значимо не отличается от нормального (p - value теста Шапиро - Уилка больше 0.05).
# Напишите функцию smart_lm, которая получает на вход data.frame 
# с произвольным числом количественных переменных. 
# Первая колонка в данных - это зависимая переменная, все остальные - предикторы. 
# На первом этапе вы должны отобрать предикторы для модели.
# Функция возвращает в виде вектора коэффициенты линейной регрессии 
# построенной только для отобранных предикторов 
# (условие нормальности распределения). Если таких предикторов 
# в данных не оказалось, то функция возвращает предупреждение 
# "There are no normal variables in the data".

smart_lm <- function(x){
  in_var <- data.frame(x[, 2:ncol(x)])
  dep_var <- data.frame(x[, 1])
  temp <- apply((in_var), 2, function(x) shapiro.test(x))
  vec <- lapply(temp, function(x) x$p.value>0.05)
  vec1 <- sapply(vec, function(x) (x[1]))
  in_var_ready <- data.frame(in_var[, vec1])
  if (ncol(in_var_ready) > 0) {
    ready <- cbind(dep_var, in_var_ready)
    lm_stat <- lm(ready[[1]] ~ ., ready[-1])
    return(lm_stat$coefficients)
  } else {
    return("There are no normal variables in the data")
  }
}
test_data_3 <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")
smart_lm(test_data_3)
test_data_4 <- as.data.frame(list(V1 = c(18.6, 20.9, 20.1, 19.8, 17.4, 17.9, 17.7, 24.2, 19.3, 20.8, 20.4, 19.9, 22.5, 22.1, 21.2, 18.1, 18.1, 23.3, 19, 19.6, 18.5, 21.9, 19.9, 16.8, 22.1, 19, 21.3, 19.8, 19.6, 20.3), V2 = c(19.1, 20.7, 22.2, 20.7, 20.7, 19.9, 19.1, 19.1, 22.2, 23.5, 17.9, 17.9, 19.8, 20.4, 24.1, 20.1, 21.2, 19.1, 18.3, 17.9, 17.9, 19.9, 19.8, 20.9, 18.1, 17.8, 20.4, 17.6, 21.6, 16.2)))
smart_lm(test_data_4)

# Иногда возникает необходимость применить какой-либо критерий к 
# большому числу количественных переменных. В этой задаче мы 
# используем выборочный t - test, который сравнивает выборочное 
# среднее с предполагаемым средним в генеральной совокупности. 
# Напишите функцию one_sample_t, которая получает на вход два 
# аргумента:
# 1. Dataframe произвольного размера с произвольным числом 
# переменных различного типа.
# 2. Числовое значение среднего в генеральной совокупности.
# Ваша функция должна применять одновыборочный t - test к каждой 
# числовой переменной в данных, и сравнивать среднее значение 
# этой переменной с указанным значением среднего в генеральной 
# совокупности (второй аргумент функции).
# Функция должна возвращать список, где каждый элемент 
# это вектор, состоящий из t - значения, числа степеней свобод 
# (df) и значения p - value.
test_data <- as.data.frame(list(V1 = c("B", "A", "A", "A", "A", "B", "A", "B", "A", "A"), V2 = c("B", "A", "A", "B", "A", "A", "A", "A", "B", "B"), V3 = c(39, 34, 43, 26, 27, 36, 34, 49, 25, 36), V4 = c(40, 27, 31, 39, 34, 44, 30, 42, 41, 30), V5 = c(29, 37, 48, 25, 43, 44, 25, 50, 22, 44)))

one_sample_t <- function(test_data, general_mean){
  nums <- unlist(lapply(test_data, is.numeric), use.names = FALSE) 
  return(lapply(test_data[, nums], function(x) c(t.test(x,mu=general_mean)$statistic, t.test(x,mu=general_mean)$parameter, t.test(x,mu=general_mean)$p.value)))
}

# Рассмотрим еще одну задачу, которая часто возникает
# при анализе данных. Допустим у вас есть список, 
# состоящий из нескольких списков (например, результаты проверки 
# на нормальность различных переменных):
# > test_1 <- shapiro.test(iris$Sepal.Length)
# > test_2 <- shapiro.test(iris$Sepal.Width)
# > test_3 <- shapiro.test(iris$Petal.Length)
# > test_4 <- shapiro.test(iris$Petal.Width)
# > normality_tests <- list(test_1, test_2, test_3, test_4)
# также мы могли получить такой список при помощи команды:
# > normality_tests <- lapply(iris[, 1:4], shapiro.test)
# Однако часто для решения локальных задач нам может 
# понадобиться только некоторые элементы из списков. 
# В этой задаче мы хотели бы вытащить из каждого списка
# только значение p - value. То есть получить новый 
# список или вектор только с одним показателем 
# (p - value) для каждой переменной. 
# > normality_tests_p
# $p.value
# [1] 0.01018116

# $p.value
# [1] 0.1011543

# $p.value
# [1] 7.412263e-10

# $p.value
# [1] 1.680465e-08
# Итак, ваша задача, написать функцию get_p_value, которая получает 
# на вход список (назовем его главным списком), каждый элемент этого
# списка тоже список - результат выполнения функции shapiro.test 
# (смотри пример normality_tests). Ваша задача из каждого элемента
# главного списка вытащить только p - value. В итоге функция возвращает 
# список где каждый элемент - одно значение - p - value 
# (как в примере normality_tests_p).
normality_tests <- lapply(iris[, 1:4], shapiro.test)
normality_tests[[1]]$p.value
lapply(normality_tests[1:length(normality_tests)], function(x) x[2])

str(temp1)
get_p_value <- function(test_list){
  temp1 <- lapply(test_list[1:length(test_list)], function(x) x[2])
  return(sapply(temp1[1:length(temp1)], function(x) x[1]))
}
get_p_value(normality_tests)


