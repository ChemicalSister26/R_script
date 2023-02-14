# Binomial test
# we have a coin and we flip a coin (toss a coin) 20 times, we have got 5 HEADS, 
# and 15 Tails, we want to check weither it is right coin (probability is 0.5)

binom.test(x=5, n=20, p=0.5)

#Chi-Square (Pearson Chi-Squared test)

t1 <- table(Status=df$status)


ch <- chisq.test(t1)
ch
# expected values
ch$exp
# observed values
ch$obs


t2 <- table(Status=df$status, Field=df$field)
t2

cht2 <- chisq.test((t2))
cht2


#Fisher exact test

fisher.test((t2))

# На основе таблицы HairEyeColor создайте ещё одну таблицу, 
# в которой хранится информация о распределении цвета глаз у женщин-шатенок 
# (Hair = 'Brown'). Check Chi-squared.
t4 <-  data['Brown', , 'Female']
chisq.test(t4)



data('diamonds')
da <- diamonds
tda <- table(Color=diamonds$color, Cut=diamonds$cut)
tda
tdach <- chisq.test((tda))

main_stat <- tdach$statistic


carat_mean <- mean(diamonds$carat)
price_mean <- mean(diamonds$price)
da$dimension_carat[diamonds$carat >= carat_mean] = 1
da$dimension_carat[diamonds$carat < carat_mean] = 0

da$dimension_price[diamonds$price >= price_mean] = 1
da$dimension_price[diamonds$price < price_mean] = 0

table_da = table(price=da$dimension_price, carat=da$dimension_carat)
table_da


tda_test <- chisq.test(table_da)
main_stat <- tda_test$statistic


data('mtcars')
mtcars_table <- table(AKPP=mtcars$am, Engine=mtcars$vs)
mtcars_table
f_test <- fisher.test(mtcars_table)

fisher_test <- f_test$p.value
