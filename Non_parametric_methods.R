# for estimation of distribution normality of a data 
# we should use Shapiro_Wilk test, but for great sample of data this test 
# may tell us that null hypothesis is not right while the distribution is normal
# so we should additionally use correlation test between theoretical quantiles and
# observed quantiles in   QQ-plot graph with this formula:
cor(sort(x), qnorm(ppoints(length(x))))
 # if correlation is more than 0.95 we may say that samples distributed normally.


# Самым популярным непараметрическим критерием для сравнения двух групп является 
# U-критерий Манна — Уитни. Разумно применять вместо t - теста если: 
# Распределение хотя бы в одной из выборок значительно отличается от нормального. 
# Есть заметные выбросы в данных. 
# В некоторых задачах мощность теста даже выше, чем t критерия 
# (например, когда d обеих выборках наблюдается заметная асимметрия 
# в одинаковом направлении). 
# Неразумно применять: 
  
  # Выборки разного размера, с различным направлением асимметрии.