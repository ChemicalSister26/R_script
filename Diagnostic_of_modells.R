data(swiss)
swiss

# first let's take a look on relasionships between all the variables

pairs(swiss)


# let's take a closer look on relation between Examination and Education

ggplot(swiss, aes(x=Examination, y=Education))+
  geom_point()

# let's find out whether there are some outliers

ggplot(swiss, aes(x=Examination, y=Education))+
  geom_point()+
  geom_smooth(method = 'lm')

# let's look on distribution of variables for work

ggplot(swiss, aes(x=Examination))+
  geom_histogram()

ggplot(swiss, aes(x=Education))+
  geom_histogram()

# education is skewed, we better should to get rid from it's use,
# but if we forced to do this, we should apply log to data

ggplot(swiss, aes(x=log(Education)))+
  geom_histogram()


my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
# Какое преобразование позволяет сделать его распределение нормальным (согласно shapiro.test)?

shapiro.test(sqrt(my_vector))


# Функция scale() позволяет совершить стандартизацию вектора, то есть делает
# его среднее значение равным нулю, а стандартное отклонение - единице (Z-преобразование).
# Стандартизованный коэффициент регрессии (β) можно получить, если предикторы и зависимая переменная стандартизованы.
# Напишите функцию, которая на вход получает dataframe с двумя количественными переменными,
# а возвращает стандартизованные коэффициенты для регрессионной модели,
# в которой первая переменная датафрейма выступает в качестве зависимой, а вторая в качестве независимой.



beta.coef <- function(x){
  x$scaled_DV <- scale(x[[1]])
  x$scaled_IV <- scale(x[[2]])
  fit_scaled <- lm(scaled_DV ~ scaled_IV, x)
  return(fit_scaled$coefficients)
}
beta.coef(swiss[,c(1,4)])


# linearity in relation between DV and IV
ggplot(swiss, aes(x=Examination, y=Education))+
  geom_point()+
  geom_smooth()

# let's use simple liner regression
lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)


# now it's time for polinomial regression, we will create new variavle in the dataset
swiss$Examination_squared <- swiss$Examination^2
lm_p <- lm(Education ~ Examination+Examination_squared, swiss)
summary(lm_p)

# let's compare two models
anova(lm1, lm_p)


# for estimation of quality of our models we will add columns with predicted values 
# by one model and by another model

swiss$lm_fitted <- lm1$fitted.values
swiss$lm_p_fitted <- lm_p$fitted.values
swiss$lm_resid <- lm1$residuals
swiss$lm_p_resid <- lm_p$residuals
swiss$obj_number <- 1:nrow(swiss)


# plots of fitted.values for our both models
ggplot(swiss, aes(y=Education, x=Examination))+
  geom_point()+
  geom_line(aes(x=Examination, y=lm_fitted), col='red')+
  geom_line(aes(x=Examination, y=lm_p_fitted), col='green')

ggplot(swiss, aes(x=lm_fitted, y=lm_resid))+
  geom_point()+
  geom_hline(yintercept = 0, col='red')
# we can see that residuals at the beginning are in the upper part of th eplot,
# at the middle they are mostly at the bottom part of the plot and at the very end they are 
# at the upper right corner - there is no thermostatically in residual distribution.

ggplot(swiss, aes(x=lm_p_fitted, y=lm_p_resid))+
  geom_point()+
  geom_hline(yintercept = 0, col='green')

# here we can see that residuals are at the same proportion in upper part of the plot
# and in the bottom part of the plot - better distribution



# residuals should be independent, let's check if it is so

ggplot(swiss, aes(x=obj_number, y=lm_resid))+
  geom_point()+
  geom_smooth()

ggplot(swiss, aes(x=obj_number, y=lm_p_resid))+
  geom_point()+
  geom_smooth()

# we see no clasterisation on the plots, so independence is sufficient

# Функция gvlma() из библиотеки gvlma позволяет получить оценку 
# выполнения основных допущений линейной регрессии. В качестве 
# аргумента она принимает объект, в который сохранена модель. 
# Можно задать формулу модели прямо в функции gvlma. Чтобы увидеть 
# основные статистики, нужно выполнить команду summary для объекта, 
# созданного с помощью функции gvlma.

df_task <- read.csv('https://stepic.org/media/attachments/lesson/12088/homosc.csv')
gvlma <- gvlma(lm(DV ~ IV, df_task))
summary(gvlma)


# residuals should be distributed normally

ggplot(swiss, aes(lm_resid))+
  geom_histogram()

# distribution is skewed a little

# QQ-plot
qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(swiss$lm_resid)


ggplot(swiss, aes(lm_p_resid))+
  geom_histogram()


qqnorm(lm_p$residuals)
qqline(lm_p$residuals)

shapiro.test(swiss$lm_p_resid)

# Напишите функцию resid.norm, которая тестирует распределение остатков 
# от модели на нормальность при помощи функции shapiro.test и создает 
# гистограмму при помощи функции ggplot() с красной заливкой "red", 
# если распределение остатков значимо отличается от нормального (p < 0.05), 
# и с зелёной заливкой "green" - если распределение остатков значимо не 
# отличается от нормального. На вход функция получает регрессионную модель.
# Функция возвращает переменную, в которой сохранен график ggplot.


resid.norm  <- function(fit){
  temp <-  as.data.frame(fit$residuals)
  if (shapiro.test(fit$residuals)$p.value < 0.05) {
    my_plot <-  ggplot(temp, aes(x=fit$residuals)) +
      geom_histogram(fill='red')
    
  } else {
    my_plot <- ggplot(temp, aes(x=fit$residuals))+
      geom_histogram(fill='green')
  }
  return(my_plot)
  }


             