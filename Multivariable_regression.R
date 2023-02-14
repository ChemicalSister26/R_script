# linear regression with multiple independent variables
data(swiss)
swiss <-  data.frame(swiss)

str(swiss)

# let's take a look on dependent variable - Fertility
hist(swiss$Fertility, col='red')

l_m1 <- lm(Fertility ~ Examination+Catholic, swiss)
summary(l_m1)

l_m1_int <- lm(Fertility ~ Examination*Catholic, swiss)
summary(l_m1_int)


# confidential interval

confint(l_m1_int)

# Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
# x_1  -  числовой вектор
# x_2 - числовой вектор
# y - числовой вектор с пропущенными значениями.
# Теперь — самое интересное. На первом этапе, используя только наблюдения, 
# в которых нет пропущенных значений, мы построим регрессионную модель 
# (без взаимодействий), где  y — зависимая переменная, x_1 и x_2 — независимые 
# переменные. Затем, используя построенную модель, мы заполним пропущенные 
# значения предсказаниями модели. Функция должна возвращать dataframe c 
# новой переменной  y_full. Сохраните в нее переменную y, в которой 
# пропущенные значения заполнены предсказанными значениями построенной модели.

test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")


fill_na <- function(x){
  model <- lm(y ~ x_1+x_2, x)
  na_subset <- subset(x, is.na(x$y))
  na_subset_full <- subset(x, !is.na(x$y))
  na_subset_full$y_full <- na_subset_full$y
  na_subset$y_full <- predict(model, na_subset)
  res <- rbind(na_subset, na_subset_full)
  return(res[order(as.numeric(rownames(res))),,drop=FALSE])
}

fill_na(test_data)


# В переменной df сохранен subset данных mtcars только с переменными "wt", 
# "mpg", "disp", "drat", "hp". Воспользуйтесь множественным регрессионным 
# анализом, чтобы предсказать вес машины (переменная "wt").
# Выберите такую комбинацию независимых переменных (из "mpg", "disp", 
# "drat", "hp"), чтобы значение
# R^2 adjusted было наибольшим. Взаимодействия факторов учитывать не надо. 

df <-  mtcars[,c('wt', 'mpg', 'disp', 'drat', 'hp')]
test1 <- lm(wt ~ mpg + disp + drat + hp, df)
summary(test1)
# Adjusted R-squared:  0.8374 
test2 <- lm(wt ~ mpg + disp + hp, df)
summary(test2)
# Adjusted R-squared:  0.8428 
test3 <- lm(wt ~ mpg + disp, df)
summary(test3)
# Adjusted R-squared:  0.8242 


# Воспользуйтесь встроенным датасетом attitude, чтобы предсказать рейтинг
# (rating) по переменным complaints и critical. Каково t-значение для 
# взаимодействия двух факторов?

data('attitude')
attitude
test4 <- lm(rating ~ complaints*critical, attitude)
summary(test4)
 # answer is 0.316

# linear regression with categorical variables
ggplot(swiss, aes(x=Catholic, fill='red'))+
  geom_histogram( binwidth=15)

# we can see that the distribution of Catholic variable is looks like
# binomial, let's decrease dimensions and create variable with two levels 
# - 'low' and 'many' of catholics

swiss$religion <- ifelse(swiss$Catholic > 60, 'Many', 'Low')
swiss$religion <-  as.factor(swiss$religion)


l_m_cat <- lm(Fertility ~ Examination+religion, data=swiss)
summary(l_m_cat)

ggplot(swiss, aes(x = Examination, y = Fertility, col=religion)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

l_m_cat_3 <- lm(Fertility ~ Examination*religion*Infant.Mortality, data=swiss)
summary(l_m_cat_3)


mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
l_m_mtcars <- lm(mpg ~ wt*am, mtcars)
summary(l_m_mtcars)
my_plot <- ggplot(mtcars, aes(x = wt, y = mpg, col=am)) + 
  geom_smooth(method = 'lm')

# optimization of models
data(swiss)
str(swiss)
# model with all the predictors
test_full <- lm(Fertility ~ ., swiss)
summary(test_full)
# Multiple R-squared:  0.7067 Adjusted R-squared:  0.671


# let's remove one predictor
test_reduced <-  lm(Fertility ~ Agriculture + Examination + Education + Catholic, swiss )
summary(test_reduced)
# Multiple R-squared:  0.6498,	Adjusted R-squared:  0.6164

test_reduced1 <-  lm(Fertility ~ Agriculture + Infant.Mortality + Education + Catholic, swiss )
summary(test_reduced1)
# Multiple R-squared:  0.6993,	Adjusted R-squared:  0.6707 
# what is the best model? насколько значимо различаются доли дисперсий, обьясняемые разными моделами можно 
# применить следующий анализ
anova(test_full, test_reduced)
# F=7.9612 p_value=0.007336 ** that means the part of variance explained by full model is 
# significantly more than variance explained by shortened model.

# for not to look over a lot of combinations we may use the function step
optimal_fit <- step(test_full, direction='backward')
summary(optimal_fit)


data(attitude) 
attitude
# all_predictors
model_full <- lm(rating ~ ., data = attitude)

# no one predictor 
model_null <- lm(rating ~ 1, data = attitude)
# Function step позволяет нам подобрать модель с оптимальным количеством предикторов.
# С помощью аргумента scope мы можем задать пространство моделей с разным числом предикторов, 
# в котором будет происходить поиск оптимального набора предикторов. 
# Самый простой путь - задать границы возможных моделей с помощью нулевой и полной моделей.
scope = list(lower = model_null, upper = model_full)

ideal_model <- step(object = model_full, scope = scope, direction='backward')
summary(ideal_model)

# Сравните полную модель из предыдущего степа и оптимальную модель с помощью функции anova. 
# Введите получившееся F-значение.
anova(model_full, lm(formula = rating ~ complaints + learning, data = attitude))

# напишите команду, которая создаёт линейную регрессию с главными эффектами 
# и всеми возможными взаимодействиями второго уровня

data(LifeCycleSavings)
LifeCycleSavings
full_model <- lm(sr ~ (pop15+pop75+dpi+ddpi)^2, LifeCycleSavings)
