my_calc <- function(x, y){
  s <- x+y
  return(s)
}

my_calc(2,3)

my_calc1 <- function(x, y) {
  s <- x + y
  d <- x - y
  return(c(s,d))
}
my_calc1(15,10)


distr <-  rnorm(100)
hist(distr)


distr[1:30] <- NA
distr
is.na(distr)
distr[is.na(distr)] <- mean(distr, na.rm = TRUE)
distr

source('my_func.R')

stok <-  c('a', 'b', 'w')
mean_func(distr)
mean_func(stok)

my_vector <- c(1, 2, 3, NA, NA)
temp <- c()
for (i in 1:length(my_vector)) {
    if (is.na(my_vector[i])) {
      temp <- append(temp, i)
}
}

NA.position <- function(x) {
  temp <- 0
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      temp <- temp + 1
    }
  }
  return(temp)
}

filtered.sum <- function(x) {
  temp <- 0
  for (i in 1:length(x)) {
    if (x[i] > 0 & !is.na(x[i])) {
      temp <-  temp + x[i]
    }
    }
return(temp)}

filtered.sum(c(1, -2, 3, NA, NA))
