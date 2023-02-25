# Напишите функцию smart_test, которая получает на вход dataframe с 
# двумя номинативными переменными с произвольным числом градаций. 
# Функция должна проверять гипотезу о независимости этих двух переменных 
# при помощи критерия хи - квадрат или точного критерия Фишера.
# Если хотя бы в одной ячейке таблицы сопряженности двух переменных меньше 
# 5 наблюдений, функция должна рассчитывать точный критерий Фишера и
# возвращать вектор из одного элемента: получившегося p - уровня значимости.
# Если наблюдений достаточно для расчета хи-квадрат 
# (во всех ячейках больше либо равно 5 наблюдений), 
# тогда функция должна применять критерий хи-квадрат 
# и возвращать вектор из трех элементов: 
# значение хи-квадрат, число степеней свободы,  p-уровня значимости.

smart_test <-  function(x){
  subset_table <- table(x)
  if (min(subset_table)<5) {
    fisher_test <- fisher.test(subset_table)
    return(fisher_test$p.value)
  }
  else {
    chi_test <- chisq.test(subset_table)
    return(c(chi_test$statistic, chi_test$parameter, chi_test$p.value))
  }
}

# Вся наследственная информация в живых организмах хранится 
# внутри молекулы ДНК. Эта молекула состоит из последовательности четырех
# "букв" — A, T, G и C. 
# Напишите функцию most_significant, которая получает на вход dataframe
# с произвольным количеством переменных, где каждая переменная 
# это нуклеотидная последовательность. Для каждой переменной мы можем проверить 
# нулевую гипотезу о том, что все нуклеотиды (A, T, G, C)
# встречаются равновероятно внутри этой последовательности. 
# Однако, возможно, что в некоторых последовательностях 
# распределение частоты встречаемости каждого нуклеотида отличается от равномерного.
# Функция должна возвращать вектор с ﻿названием переменной (или переменных),
# в которой был получен минимальный p - уровень значимости при проверке
# гипотезы о равномерном распределении нуклеотидов при помощи критерия хи - квадрат. 
test_data_smooth <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)

most_significant <-  function(x){
  res_n <- apply(x, 2, function(x) chisq.test(table(x))$p.value)
  min <- min(as.vector(res_n))
  as.list(res_n)
  names <-  colnames(x)
  names[which(res_n == min)]
}

# иногда возникает необходимость перекодировать количественную переменную 
# в номинативную. Однако зачастую мы можем создавать новую номинативную 
# переменную, комбинируя значения нескольких количественных переменных.
# Создайте iris новую переменную important_cases - фактор с двумя градациями 
# ("No" и "Yes"). Переменная должна принимать значение Yes, если для данного
# цветка значения хотя бы трех количественных переменных выше среднего.
# В противном случае переменная important_cases  будет принимать значение No.

library(dplyr)
data(iris)
means <- lapply(iris[1:4], function(x) mean(x))

s <- as.data.frame(mutate_all(iris[1], .funs = function(x) (ifelse(x > means[1], 1, 0))))
sw <- as.data.frame(mutate_all(iris[2], .funs = function(x) (ifelse(x > means[2], 1, 0))))
p <- as.data.frame(mutate_all(iris[3], .funs = function(x) (ifelse(x > means[3], 1, 0))))
pl <- as.data.frame(mutate_all(iris[4], .funs = function(x) (ifelse(x > means[4], 1, 0))))
all <- cbind(s, sw, p, pl)

all$sum <- as.data.frame(all$Sepal.Length + all$Sepal.Width + all$Petal.Width + all$Petal.Length)
important_cases <- as.data.frame(mutate_all(all$sum, 
                                            .funs = function(x) as.factor(ifelse(x > 2, 'Yes', 'No'))))
colnames(important_cases)[1] = 'important_cases'
iris <- cbind(iris, important_cases)

# or much shorter way

importance_calc <- function(v1, v2, threshold=3){    
  ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])))

# Обобщим предыдущую задачу! Напишем функцию get_important_cases, 
# которая принимает на вход dataframe с произвольным числом количественных 
# переменных (гарантируется хотя бы две переменные). Функция должна возвращать 
# dataframe с новой переменной - фактором important_cases.
# Переменная  important_cases принимает значение Yes, если для данного наблюдения 
# больше половины количественных переменных имеют значения больше среднего. 
# В противном случае переменная important_cases принимает значение No.
# Переменная  important_cases - фактор с двумя уровнями 0 - "No", 1  - "Yes". 
# То есть даже если в каком-то из тестов все наблюдения получили значения "No", 
# фактор должен иметь две градации. 

test_data_th <- as.data.frame(list(V1 = c(24, 30, 16, 18, 8),
                                   V2 = c(16, 22, 23, 12, 22), 
                                   V3 = c(22, 17, 14, 18, 20)))

importance_calc <- function(v1, v2, threshold=ncol(test_data_th)/2){    
  ifelse(sum(v1 > v2) > threshold, 'Yes', 'No')} 
test_data_th$important_cases <- factor(apply(test_data_th, 1, 
                                importance_calc, v2 = colMeans(test_data_th)), levels = c("No", "Yes"))
                           
str(test_data_th)

get_important_cases <- function(x){
  importance_calc <- function(v1, v2, threshold=ncol(x)/2){    
  ifelse(sum(v1 > v2) > threshold, 'Yes', 'No')} 
  x$important_cases <- factor(apply(x, 1, importance_calc, v2 = colMeans(x)), levels = c("No", "Yes"))
  return(x)
}
res <- get_important_cases(test_data_th)
str(res)

# Задачка на программирование.
# В R мы без труда можем рассчитать среднее и медиану вектора, 
# а вот встроенной функции для расчета моды — наиболее часто
# встречаемого значения — в R нет! А мода так бы пригодилась 
# нам при анализе номинативных данных!
# Напишите функцию stat_mode, которая получает на вход вектор 
# из чисел произвольной длины и возвращает 
# числовой вектор с наиболее часто встречаемым значением. 
# Если наиболее часто встречаемых значений несколько, 
# функция должна возвращать несколько значений моды  в виде числового вектора. 

v <- c(1, 1, 1, 2, 3, 3, 3)

stat_mode <- function(x){
  tab <- table(x)
  names <- names(tab)
  as.numeric(names(which(tab == max(tab))))
}
stat_mode(v)

# Доктор Пилюлькин решил вооружиться статистикой, чтобы сравнить 
# эффективность трех лекарств! Давайте поможем ему и напишем функцию 
# max_resid, которая получает на вход dataframe с двумя переменными: 
# типом лекарства и результатом его применения. 
# Drugs - фактор с тремя градациями: drug_1, drug_2, drug_3.     
# Result - фактор с двумя градациями: positive, negative.
# Функция должна находить ячейку таблицы сопряженности с 
# максимальным  значением стандартизированного остатка и 
# возвращать вектор из двух элементов: название строчки и 
# столбца этой ячейки.
# Для расчета стандартизированных остатков вы можете 
# воспользоваться уже знакомой вам функцией chisq.test(). 
# Изучите справку по этой функции, чтобы найти, 
# где хранятся стандартизированные остатки.


test_data_pil <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
max_resid <- function(x){
  df_pil <- table(x)
  ch_test <- chisq.test(df_pil)
  max_resid <- max(ch_test$stdres)
  ind <- which(ch_test$stdres == max(max_resid), arr.ind = T)
  return(c(rownames(df_pil)[ind[1]], colnames(df_pil)[ind[2]]))
}

max_resid(test_data_pil)

# visualise diamonds library
data(diamonds)
ggplot(diamonds, aes(x=color, fill=cut))+
  geom_bar(position = position_dodge())