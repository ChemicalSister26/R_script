# Напишите функцию find_outliers, которая получает на вход dataframe 
# с одной количественной переменной и произвольным числом факторных переменных. 
# Факторные переменные разбивают все наши наблюдения на определенное число групп. 
# Например, если мы посмотрим на данные mtcars и возьмем в качестве
# группирующих переменных: am - две градации и cyl три градации, то получим
# 6 групп наблюдений на пересечении градаций этих переменных. 
# Итак, ваша задача — создать в данных новую числовую переменную is_outlier, 
# которая будет принимать значение 1, 
# если наблюдение в этой строке является выбросом в своей группе, и 0, если не является.
# Под выбросами будем понимать наблюдения, отклоняющиеся от среднего значения 
# в группе более чем на два стандартных отклонения этой группы. 

test_data_ap <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
fact_var <- as_tibble(lapply(select(test_data_ap, !where(is.numeric)), 
                             function(x) as.factor(x)))
num_var <- select(test_data_ap, where(is.numeric))
test_data_ap <- cbind(fact_var, num_var)
gr_test <- group_by_at(test_data_ap, names(fact_var))
summ <- gr_test %>% 
  summarise(m=mean(x), s=sd(x))
summ$ms <-  summ$m + 2*summ$s
summ$negms <- summ$m - 2*summ$s
names <- names(num_var)
names1 <- c('ms', 'negms')
ready <- append(names, names1)
select(joined_df, all_of(ready))
joined_df <- left_join(gr_test, summ, by = c("factor_2" = "factor_2"))
joined_df$is_outlier <- sapply(select(joined_df, where(is.numeric)), function(x) ifelse(x<joined_df$negms| x>joined_df$ms, 1, 0))
mutate(joined_df, .funs = function(x) )