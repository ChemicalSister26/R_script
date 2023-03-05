# Вся наследственная информация в живых организмах хранится внутри молекулы ДНК. 
# Эта молекула состоит из последовательности четырех "букв" — A, T, G и C.
# Напишите функцию most_significant, которая получает на вход dataframe с 
# произвольным количеством переменных, где каждая переменная это нуклеотидная последовательность. 
# Для каждой переменной мы можем проверить нулевую гипотезу о том, что все нуклеотиды (A, T, G, C) 
# встречаются равновероятно внутри этой последовательности. Однако, возможно, что 
# в некоторых последовательностях распределение частоты встречаемости каждого 
# нуклеотида отличается от равномерного.


test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)


most_significant <-  function(x){
  res_n <- apply(x, 2, function(x) chisq.test(table(x))$p.value)
  min <- min(as.vector(res_n))
  as.list(res_n)
  names <-  colnames(x)
  names[which(res_n == min)]
}

most_significant(test_data)


# иногда возникает необходимость перекодировать количественную переменную
# в номинативную. Однако зачастую мы можем создавать новую номинативную переменную, 
# комбинируя значения нескольких количественных переменных. Рассмотрим такой пример.
# Воспользуемся встроенными в R данными Iris. Они сразу доступны для работы. 
# Создайте новую переменную important_cases - фактор с двумя градациями ("No" и "Yes"). 
# Переменная должна принимать значение Yes, если для данного цветка значения хотя 
# бы трех количественных переменных выше среднего. В противном случае переменная
# important_cases  будет принимать значение No.

data(iris)
means <- lapply(iris[1:4], function(x) mean(x))
colMeans(iris[1:4])
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
iris$important_cases <- factor(apply(iris[1:4], 1, 
                                     importance_calc, v2 = colMeans(iris[, 1:4])))



# Обобщим предыдущую задачу! Напишем функцию get_important_cases, 
# которая принимает на вход dataframe с произвольным числом количественных 
# переменных (гарантируется хотя бы две переменные). Функция должна возвращать 
# dataframe с новой переменной - фактором important_cases.
# Переменная  important_cases принимает значение Yes, если для данного наблюдения
# больше половины количественных переменных имеют значения больше среднего. 
# В противном случае переменная important_cases принимает значение No.
# Переменная  important_cases - фактор с двумя уровнями 0 - "No", 
# 1  - "Yes".  То есть даже если в каком-то из тестов все наблюдения
# получили значения "No", фактор должен иметь две градации. 

test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

importance_calc <- function(v1, v2, threshold=3){    
  ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
test_data$important_cases <- factor(apply(test_data, 1, 
                                     importance_calc, v2 = colMeans(test_data)))