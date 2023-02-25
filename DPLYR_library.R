# here we create dplyr dataframe
data_dp <- data_frame(x=rnorm(10000), y=rnorm(10000), f=factor(rep(1:2, 5000)))
head(data_dp) 
# now let's create basic dataframe to see difference between them.
data_n_dp <- data.frame(x=rnorm(10000), y=rnorm(10000), f=factor(rep(1:2, 5000)))

# dplyr dataframe print in console only 10 first rows
data_dp


# we may convert existing in R data in DPLYR dataframe
diamonds <- as_data_frame(diamonds)

# we can manipulate created variables right during the dataframe creation
my_data_2 <- data_frame(x = rnorm(10), y=x*2, z=abs(x))

# while basic dataframe creation throws an error
my_data_c <- data.frame(x = rnorm(10), y=x*2, z=abs(x))
#Error in data.frame(x = rnorm(10), y = x * 2, z = abs(x)) : 
#  arguments imply differing number of rows: 10, 32

# selection of variables
select(diamonds, cut, price)
# equals to
diamonds[, c('cut', 'price')] 
# all the columns between 'cut' and 'price'

select(diamonds, cut:price)

select(diamonds, -cut) # all the columns besides 'cut'


# choose only that columns that start with letter "c"
select(diamonds, starts_with('c'))
# choose only that columns that end with letter "t"
select(diamonds, ends_with('t'))
# choose only that columns that contains letter "t"
select(diamonds, contains('t'))


# what will we do if we are looking for rows? Use slice option

slice(diamonds, 2)
slice(diamonds, 2:10)
slice(diamonds, c(1, 2, 5, 8)) 


# we can make any subset with the given conditions using Filter

filter(diamonds, carat>0.3, color=='J')
# the same with the conventional language
diamonds[diamonds$carat>0.3 & diamonds$color=='J', ]


arrange(diamonds, price) # sorts dataset by price in ascending type
arrange(diamonds, price, depth)

diamonds[order(diamonds$price), ]


# rename columns
rename(diamonds, new_cut = cut)

ggplot(diamonds, aes(x=price, y=carat))+
  geom_point()

# let's transform variable price MUTATE a.k.a. MUTATION
m <- mutate(diamonds, sqrt_price = sqrt(price))

ggplot(m, aes(x = sqrt_price, y=carat))+
  geom_point()


# В переменную d сохраните только нeчетные строчки 
# исходных данных diamonds. Обратите внимание на функцию seq(). 
# Она может вам пригодиться вам не только в этой задаче.
nrow(diamonds)
d <- slice(diamonds, seq(from = 1, to = nrow(diamonds), by = 2))


# D пакете dplyr есть оператор %>% (Ctrl + Shift + m), 
# при помощи которого можно отправить результат выполнения 
# одной команды в следующую, смотрите, как это работает.
# iris %>% 
#  filter(Petal.Length > 1.7) %>% 
#  arrange(Sepal.Length) %>% 
#  select(Sepal.Length, Sepal.Width)
# Оператор %>% в прямом смысле слова перетаскивает то, 
# что написано перед ним в качестве аргумента, 
# в следующую функцию. На первом этапе мы отправляем данные 
# iris в функцию filter, в которой теперь достаточно указать 
# только условие фильтрации. Далее результат фильтрации 
# мы отправляем на сортировку по переменной Sepal.Length. 
# И на последнем этапе уже отфильтрованные и отсортированные
# данные мы отправляем в функцию select, чтобы отобрать нужные колонки.
#
#
# Потренируемся использовать изученные функции.
# Из данных mtcars отберите только четыре переменные: 
# mpg, hp, am, vs. Оставьте только те наблюдения, 
# для которых значения mpg > 14 и hp > 100. 
# Отсортируйте получившиеся данные по убыванию
# переменной mpg и возьмите только первые 10 строчек.
# Переменную mpg переименуйте в Miles per gallon, 
# а переменную hp в  Gross horsepower 
# (обратите внимание, dplyr позволит нам 
# создать пременные с пробелами в названии). 
# Получившийся dataframe сохраните в переменную my_df.
data(mtcars)
my_df <- rename(slice(arrange(filter(select(mtcars, mpg, am, hp, vs), mpg>14 & hp>100), 
        desc(mpg)), 1:10), 'Miles per Gallon'='mpg', 'Gross horsepower' = 'hp')


my_df1 <- mtcars %>% 
  filter(mpg>14 & hp>100) %>%
  arrange(desc(mpg)) %>%
  rename('Miles per Gallon'='mpg', 'Gross horsepower' = 'hp') %>% 
  slice(1:10) %>%
  select('Miles per Gallon', am, vs, 'Gross horsepower')



# for each column mutation in dataset we may use MUTATE_EACH function

dfm <- as_data_frame(matrix(rnorm(30), ncol=5))

mutate_each(dfm, funs = abs)



# Напишите функцию, all_to_factor, которая преобразует dataframe, 
# переводя все его переменные в фактор.
all_to_factor <- function(x) {
  as_data_frame(sapply(mtcars,  function(x) x=as.factor(x), simplify = F))
}

# Написать функцию, которая получает на вход dataframe  
# с произвольным числом переменных разных типов. 
# На первом этапе функция должна выполнить предобработку 
# числовых переменных. Т.к. значение логарифма мы можем 
# рассчитать только для положительных чисел. 
# Для этого сделаем центрирование всех переменных (Rescaling), 
# только еще добавим единичку, чтобы у нас не осталось нулей.
# После того как мы масштабировали каждую переменную, 
# осталось рассчитать значение натурального логарифма
# каждого наблюдения (функция log) и вернуть новый dataframe. 

test_data <- as_tibble(list(V1= c(1.5, -0.1, 2.5, -0.3, -0.8),
                                V2= c(-0.9, -0.3, -2.4, 0.0,  0.4),
                                V3= c(-2.8, -3.1, -1.8, 2.1, 1.9),
                                V4 =c('A', 'B', 'B', 'B', 'B')))
log_transform <- function(test_data){
temp_num <- as_tibble(apply(select(test_data, where(is.numeric)), 2, function(x) rescale(x)))
temp_1 <- mutate_all(mutate_all(temp_num, .funs = function(x) x+1), .funs = function(x) log(x))
res <- cbind(temp_1, select(test_data, !where(is.numeric)))
return(res)}
log_transform(test_data)


# GROUP-BY

data(diamonds)

gr_data <- group_by(diamonds, cut)
 # let's choose random data from grouped dataset

sample_n(gr_data, 2)


# SUMMARIZE

summarise(mtcars, mean(disp))

summarise(diamonds, mean(price))
# `mean(price)` 3933.

summarise(gr_data, mean(price))
#     cut       `mean(price)`
#   <ord>             <dbl>
#  1 Fair              4359.
#  2 Good              3929.
#  3 Very Good         3982.
#  4 Premium           4584.
#  5 Ideal             3458.

summarise(gr_data, mean(price), median(y), max(x))


test_data_2 <- as_tibble(read.csv("https://stepic.org/media/attachments/course/724/salary.csv"))

descriptive_stats <- function (dataset){
  gr_test <- group_by(dataset, gender, country)
  summarise(gr_test, n=n(), mean=mean(salary, na.rm=T), sd=sd(salary, na.rm=T),
            median=median(salary, na.rm=T), first_quantile=quantile(salary, 0.25, na.rm=T),
            second_quantile=quantile(salary, 0.5, na.rm=T),
            third_quantile=quantile(salary, 0.75, na.rm=T),
            na_values=sum(is.na(salary)))
}

descriptive_stats(test_data_2)


# Напишите функцию, to_factors, которая получает на вход dataframe  с 
# произвольным числом количественных переменных и вектор с номерами колонок, 
# которые нужно перевести в фактор.
# Для перевода числовых колонок в фактор будем использовать следующий принцип, 
# если наблюдение больше среднего всей переменной то 1, иначе 0.

to_factors <- function(test_data, factors){
  temp_nf <- as.data.frame(select(test_data, !factors))
  tem_mut <- mutate_all(as.data.frame(select(test_data, factors)),
                        .funs = function(x) (as.factor(ifelse(x > mean(x), 1, 0))))
  return(cbind(temp_nf, tem_mut))
}


# Применим полученные знания на практике. Возьмем данные diamonds для работы 
# в этой задаче. Создайте новый dataframe с именем high_price, в котором будут
# хранится только 10 самых дорогих бриллиантов каждого цвета. Также в итоговом 
# dataframe должны храниться только две переменные color и price.

data(diamonds)

high_price <- 
  diamonds %>% 
  select(color, price) %>% 
  group_by(color) %>% 
  arrange(color, desc(price)) %>% 
  slice(1:10)
print(temp, n=70)
