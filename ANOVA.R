# ANOVA interactions and dependence types

# DV - dependent variable
# IV - independent variable

DV ~ IV # one-way - one dependent affected by one independent
DV ~ IV1 + IV2 # two-way - one dependent affected by two independent (and not 
# dependent between each other variables)

DV ~ IV1:IV2 # one dependent affected by two dependent between each other variables
DV ~ IV1 + IV2 + IV1:IV2 # both effects formula with main effects plus interactions
DV ~ IV1*IV2 # analogue of above formula
DV ~ (IV1 + IV2 + IV3)^2 #the same as IV1+IV2+IV3+IV1:IV2+IV2:IV3+IV1+IV3


stat <- read.csv('shops.csv')
str(stat)


# one-way anova DV ~ IV
boxplot(price ~ origin, stat)
ggplot(stat, aes(x=origin, y=price))+
  geom_boxplot()

          # analysis itself
# one-way ANOVA - dependent variable - PRICE, independent variable - ORIGIN
fit <-  aov(price ~ origin, stat)
summary(fit)
# F=6.65 p=0.0189 это значит что мы отвергаем нулевую гипотезу о том,
# что цены на российские и импортные продукты равны
# we can reject null hypothesis that prices for Russian and import products
# are equal and take alternative one



# two-way anova DV ~ IV1 + IV2 DV - PRICE, independent - ORIGIN + STORE

fit1 <- aov(price ~ origin+store, stat)
summary(fit1)
# origin       1  94107   94107   6.355  0.022
# store        1   2981    2981   0.201  0.659
# we may say that ORIGIN significantly influences on  prices while STORE is not
# meaningful for them.

model.tables(fit1, 'mean')
# here we can see means of compared groups


# let's visualize obtained data
ggplot(stat, aes(x=store, y=price, color=origin, group=origin))+
  stat_summary(fun.data=mean_cl_boot, geom='errorbar', width=0.2, position=position_dodge(0.1))+
  stat_summary(fun.data=mean_cl_boot, geom='line', position=position_dodge(0.1))+
  stat_summary(fun.data=mean_cl_boot, geom='point', position=position_dodge(0.1))+
  theme_bw()

# our visualization tells us that there is difference between supermarkets and
# minimarket in pricing between Russian and import products, that is - we should
# check interaction between store type and origin, namely - store:origin

# interaction ANOVA DV ~ IV1+IV2 + IV1:IV2

fit3 <- aov(price ~ origin+store + origin:store, stat)
summary(fit3)
# we can see that interaction is meaningful too
#              Df Sum Sq Mean Sq F value Pr(>F)
# origin:store  1  62777   62777   5.315 0.0349
#


fit4 <- aov(price ~ origin*store, stat)
summary(fit4)

# Воспользуемся встроенными данными npk, 
# иллюстрирующими влияние применения различных удобрений на урожайность гороха
# (yield). Нашей задачей будет выяснить, существенно ли одновременное 
# применение азота (фактор N) и фосфата (фактор P).
# Примените дисперсионный анализ, где будет проверяться влияние фактора 
# применения азота (N), влияние фактора применения
# фосфата (P) и их взаимодействие
# В ответе укажите p-value для взаимодействия факторов N и P

data('npk')
har <- npk

ggplot(har, aes(x=N, y=yield))+
  geom_boxplot()
ggplot(har, aes(x=P, y=yield))+
  geom_boxplot()


# two-way ANOVA
fit5 <-  aov(yield~N+P+N:P, har)
summary(fit5)

# раличаются ли группы еды между собой? Хлеб, сыр, шоколад, фрукты и т.д.

ggplot(stat, aes(x=food, y=price))+
  geom_boxplot()

fit6 <- aov(price ~ food, stat)
summary(fit6)
# food         4 165823   41456   3.398 0.0362 *
# еда - значимый предиктор - различия по цене значимые
# но какие именно группы статистически значимо различаются?
# здесь поможет критерий Тьюки

TukeyHSD(fit6)
# cheese-bread          278.0525   36.86938 519.23562 0.0204058
# здесь значимые различия только между сыром и хлебом

# Проведите однофакторный дисперсионный анализ на встроенных данных iris. 
# Зависимая переменная - ширина чашелистика (Sepal.Width), 
# независимая переменная - вид (Species). Затем проведите попарные сравнения видов. 
# Какие виды статистически значимо различаются по ширине чашелистика (p < 0.05)?
#

fit7 <- aov(Sepal.Width ~ Species, iris)
summary(fit7)

# Species       2  11.35   5.672   49.16 <2e-16 ***
# различия сильно значимые между ними

TukeyHSD((fit7))
# здесь различия везде значимые

ther <- read.csv('therapy_data.csv')
str(ther)

ther$subject <- as.factor(ther$subject)
str(ther)

fit8 <-  aov(well_being ~ therapy, ther)
summary(fit8)

fit9 <-  aov(well_being ~ therapy + Error(subject/therapy), ther)
summary(fit9)

fit10 <-  aov(well_being ~ price*therapy, ther)
summary(fit10)

fit11 <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), ther)
summary(fit11)

ggplot(ther, aes(x=price, y=well_being))+
  geom_boxplot()

ggplot(ther, aes(x=price, y=well_being))+
  geom_boxplot()+
  facet_grid(~subject)

pill <- read.csv('Pillulkin.csv')
str(pill)

pill$patient <-  as.factor(pill$patient)
str(pill)
fit_pill <- aov(temperature ~ pill + Error(patient/pill), pill)
summary(fit_pill)

fit_pill1 <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), pill)
summary(fit_pill1)
            

ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group=supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))+
  theme_classic()