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
        
find_outliers <- function(t){
  fact_var <- as_tibble(lapply(select(t, !where(is.numeric)), 
                               function(x) as.factor(x)))
  gr_test <- group_by_at(t, names(fact_var))
  slice(gr_test, 1)
  summ <- summarise_all(gr_test, list(mean=mean, sd=sd))
  summ$otp <- summ$mean+2*summ$sd
  summ$otn <- summ$mean-2*summ$sd
  joined_df <- left_join(gr_test, summ, by = c("factor_2" = "factor_2"))
  joined_df$is_outlier <-  ifelse(joined_df$x > joined_df$otp | joined_df$x < joined_df$otn, 1, 0)
  return(cbind(test_data_ap, is_outlier=joined_df$is_outlier))
}
find_outliers(test_data_ap)
