# dataset description
# products.csv
# •  product_id – хеш ID товара
# •  price – стоимость товара в момент получения датасета, в копейках
# •  name - имя товара (случайная строка)
# •  available – есть ли товар в ассортименте (т.е. продается ли он на момент получения датасета)
# •  brand – бренд товара
# 
# categories.csv
# •  category_id – ID категории
# •  name – название категории
# •  parent_id – ID родительской категории, или NULL, если это верхнеуровневая категория
# •  link_url – ссылка на категорию
# 
# product-categories.csv
# •  product_id – хеш ID товара
# •  category_id – ID категории
# 
# purchases.csv
# •  ordernumber – ID заказа
# •  externalsessionid – ID сессии
# •  product_id – хеш ID товара
# •  eventdate – дата заказа
# •  timestamp – временная метка события
# •  price – стоимость единицы товара (из таблицы products), в копейках
# •  totalcents – стоимость всех купленных товаров в позиции, с учетом скидок и акций, в копейках
# •  quantity – количество купленных единиц
# 
# item-views.csv
# •  externalsessionid – ID сессии
# •  product_id – хеш ID товара
# •  duration – время просмотра, мс
# •  eventdate – дата просмотра
# •  timestamp – временная метка события
# 
# category-views.csv
# •  externalsessionid – ID сессии
# •  category_id – ID категории
# •  duration – время просмотра, мс
# •  eventdate – дата просмотра
# •  timestamp – временная метка события
# •  sessionid – ID сессии

products <- fread('products.csv')
# FREAD loads big datasets very-very-very quickly

products[1:10, ]
# equals
products[1:10]
products[!(1:10)]

 #selection from dataset
products[price > 5000] # besides 1:10 rows

products[, list(name=name,
                price_1 = price/1000)]


products[order(products$price, decreasing = T), list(name=name,
                                                     price_1 = paste0(price/1000, ' thousands'))]


# Напишите функцию filter.expensive.available, которая принимает 
# на вход products (объект типа data.table) и вектор названий брендов,
# и возвращает только те строчки, которые соответствуют товарам, цена которых 
# больше или равна 5000 рублей, доступны на складе, и принадлежат одному 
# из переданных брендов.

s_products <- data.table(price = c(10000, 600000, 700000, 1000000),
                         brand = c("a", "b", "c", "d"),
                         available = c(T, T, F, T))
brand_1 <- c("a", "c", "d")

filter.expensive.available <- function(products, brands) {
 products[(brand %in% brands) & (price >= 500000) & (available == T)]
}

filter.expensive.available(s_products, brand_1)


# Создайте функцию ordered.short.purchase.data, которая будет принимать purchases, 
# объект data.table, и возвращать таблицу только со столбцами с номером заказа и 
# ID продукта. Упорядочите результат по убыванию стоимости купленного товара. 
# Возвраты (записи с отрицательным количеством предметов в позиции) надо удалить.

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

ordered.short.purchase.data <- function(purchases) {
  purchases[order(price, decreasing = T)][quantity>0, list(ordernumber, product_id)]
}


# Напишите функцию purchases.median.order.price, у которой один аргумент: 
# purchases, и которая возвращает медианную стоимость заказа (число).
# Группировку стоит проводить с помощью data.table. Записи с неположительным
# количеством купленных товаров (возвраты) игнорировать.
# Обратите внимание, что одному заказу может соответствовать несколько записей 
# – «позиций» с одинаковым ordernumber, и что при расчете стоимости заказа 
# надо учитывать ситуации, когда пользователь купил несколько товаров одного
# типа (их количество указано в quantity).

sample.purchases.o <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

purchases.median.order.price <- function(purchases) {
  temp <- purchases[quantity>0][, list(price_all=sum((price*quantity))), by=.(ordernumber)]
  median(temp$price_all)
}

