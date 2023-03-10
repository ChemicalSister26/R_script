# Допустим, в рамках некоторого медицинского исследования тестировалась 
# эффективность новой вакцины от вируса гриппа. Одной из целей 
# исследования являлось изучить динамику температуры тела пациентов 
# на протяжении недели после вакцинации.
# Пациенты должны были каждый день приходить на осмотр, где у них 
# измеряли температуру. Каждый из семи дней экспериментатор вносил
# в таблицу id пациента и его температуру. После окончания исследования 
# выяснилось, что некоторые пациенты не смогли посетить все семь приемов. 
# Кто-то после первого раза больше не приходил на обследование, кто-то
# пропустил некоторые дни и т.д. 
# Для чистоты исследования врачам необходимо отобрать из всех 
# пациентов только тех, кто посетили каждый из семи приемов.
# Все данные хранятся в следующем формате: в списке all_data сохранены
# семь датафреймов, с двумя колонками:
#    id - закодированное имя пациента 
#    temp - значения температуры. 
# Ваша задача написать функцию get_id, которая получает на вход 
# такого рода список, состоящий из семи датафрэймов. Функция, должна 
# вернуть новый датафрэйм, в котором будут две переменные id и mean_temp 
# - среднее значение температуры за неделю только тех пациентов, 
# которые посетили все семь приемов, то есть id таких пациентов
# присутствует в каждом из семи датафреймов.

(all_data)

max <- 0
for (i in 1:7) {
    max <- max(all_data[[i]]$id)
   }

res1 <- c()
for (i in 1:7) {
temp <- as.data.frame(all_data[[i]])
  res <-  c()
  for (j in 1:max) {
    ifelse(j %in% temp$id, res <- c(res, j), res <- c(res, NA))
  }
 print(res)
 res1 <- rbind(res1, res)
}

ac_v <- as.vector(apply(as.data.frame(res1), 2, function(x) sum(is.na(x))))
g_p <- c(which(ac_v==0))
res <- data.frame(num = matrix(g_p))
for (i in 1:7) {
  temp <- as.data.frame(all_data[[i]])
  f <- filter(temp, id %in% g_p)
  res <- cbind(res, temp=f[order(f$id), ]$temp)
 }
res$num <- NA
mean_temp <- apply(res, 1, function(x) mean(x, na.rm=T))
res1 <- data.frame(id=g_p, mean_temp=mean_temp)




get_id <- function(data_list){
  max <- 0
  for (i in 1:7) {max <- max(data_list[[i]]$id) }
  res1 <- c()
  for (i in 1:7) {
    temp <- as.data.frame(data_list[[i]])
    res <-  c()
    for (j in 1:max) {
      ifelse(j %in% temp$id, res <- c(res, j), res <- c(res, NA))
    }
    res1 <- rbind(res1, res)
  }
  
  ac_v <- as.vector(apply(as.data.frame(res1), 2, function(x) sum(is.na(x))))
  g_p <- c(which(ac_v==0))
  res <- data.frame(num = matrix(g_p))
  for (i in 1:7) {
    temp <- as.data.frame(data_list[[i]])
    f <- filter(temp, id %in% g_p)
    res <- cbind(res, temp=f[order(f$id), ]$temp)
  }
  res$num <- NA
  mean_temp <- apply(res, 1, function(x) mean(x, na.rm=T))
  res1 <- data.frame(id=g_p, mean_temp=mean_temp)
  return(res1)
}

get_id(all_data)
