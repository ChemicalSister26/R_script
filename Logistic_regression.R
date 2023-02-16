df_train <-  read.csv('train.csv', sep=';')


df_train$gender <-  as.factor(df_train$gender)
df_train$hon <-  as.factor((df_train$hon))
str(df_train)

ggplot(df_train, aes(read, math, col=gender))+
  geom_point()+
  facet_grid(. ~hon)


fit <- glm(hon ~ read+math+gender, df_train, family='binomial')
summary(fit)


exp(fit$coefficients)

head(predict(object=fit))

head(predict(object=fit, type='response'))

df_train$probability <- predict(object=fit, type='response')


# Используем данные mtcars. Сохраните в переменную логистическую регрессионную модель,
# где в качестве зависимой переменной выступает тип коробки передач (am), в качестве 
# предикторов переменные disp, vs, mpg.
# Значения коэффициентов регрессии сохраните в переменную log_coef.

mtcars
task_glm <- glm(am ~disp+vs+mpg, mtcars, family = 'binomial')
log_coef <- task_glm$coefficients

# Дополните предложенный в задании код, чтобы построить следующий график по данным ToothGrowth.
# Изобразите различия длины зубов морских свинок в различных условиях дозировки и типа
# потребляемого продукта.
# По оси x - переменная supp.
# По оси y - переменная len.
# Цвет ящиков с усами (boxplot) - переменная dose.
ToothGrowth$dose <- as.factor(ToothGrowth$dose )
ggplot(data = ToothGrowth, aes(x=supp, y=len, fill=dose))+
  geom_boxplot()



# ROC-AUC curves
diff <- prediction(df_train$probability, df_train$hon)
perf_fit <- performance(diff, 'tpr', 'fpr') 
plot(perf_fit, colorize=T, print.cutoffs.at = seq(0,1, by=0.1))
# cutoffs are thresholds 

auc <- performance(diff, measure='auc')
str(auc)

# those plots will help us to find suitable - best threshold 
perf1 <- performance(diff, x.measure = 'cutoff', measure='spec')
perf2 <- performance(diff, x.measure = 'cutoff', measure='sens')
perf3 <- performance(diff, x.measure = 'cutoff', measure='acc')


plot(perf1, col='red')
plot(add=T, perf2, col='green')
plot(add=T, perf3, col='blue')
abline(v=0.225, col='yellow') # suitable threshold for our research


# so, let's create new column with categorical predicted feature with given founded threshold
df_train$hon_pred <-  factor(ifelse (df_train$probability > 0.255, 1, 0), labels = c('N', 'Y'))

df_train$correct = ifelse(df_train$hon == df_train$hon_pred, 1, 0)

ggplot(df_train, aes(x=probability, fill=factor(correct)))+
         geom_dotplot(alpha=0.4)
# let's predict numbers for new dataset

df_test <-  read.csv('test.csv', sep=';')
df_test$hon_pred <- predict(fit, newdata = df_test, type='response')


# Используем модельные данные о соотношении среднего и высшего 
# образования в американских школах. Данные доступны по ссылке:
# https://stepic.org/media/attachments/lesson/11478/data.csv Про часть 
# испытуемых известно, поступили они в университет или нет (переменная admit, 
# 1 = поступили, 0 = не поступили), про остальных таких данных нет (NA). 
# Описание данных (обратите на него внимание при проведении подсчётов).
# По имеющимся данным в переменной admit постройте логистическую регрессионную
# модель, предсказывающую результат поступления по престижности учебного 
# заведения среднего образования (переменная rank, 1 — наиболее престижное, 
# 4 — наименее престижное) и результатов GPA (переменная gpa) с учётом их 
# взаимодействия. Примените эту модель к той части данных, 
# где результат поступления неизвестен. Ответом в задаче будет предсказанное 
# моделью число поступивших из тех, для кого результат поступления был 
# неизвестен. Считаем человека поступившим, когда вероятность его поступления 
# не меньше 0.4.

resch <- read.csv('american_shools.csv')
#resch$admit <- as.factor(resch$admit)
resch$rank <- as.factor(resch$rank)
train_part <- subset(resch, !is.na(resch$admit))
test_part <- subset(resch, is.na(resch$admit))
str(train_part)

lrm <- glm(admit ~ gpa+rank+gpa*rank, train_part, family='binomial')
test_part$probability <- predict(lrm, newdata = test_part, type='response')
test_part$admittance_pred <-  (ifelse (test_part$probability >= 0.4, 1, 0))
sum(test_part$admittance_pred)
