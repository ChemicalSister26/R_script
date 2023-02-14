mean_func <-  function(x){
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)}
  else {
    print('strings')
  }
}

x <- c(2, 5, 10, 11, 10, 13, 15, 7, 9, 15, 25)
boxplot(x)
vec_q <- quantile(x, probs = c(0.25, 0.75))
upper <- vec_q[2] + IQR(x)*1.5
lower <- vec_q[1] - IQR(x)*1.5

for (i in 1:length(x)) {
  if (x[i] > upper) {
    x[i] = NA
  }
}
res <- x[!is.na(x)]
res

outliers.rm <- function(x){
  vec_q <- quantile(x, probs = c(0.25, 0.75))
  upper <- vec_q[2] + IQR(x)*1.5
  lower <- vec_q[1] - IQR(x)*1.5
  
  for (i in 1:length(x)) {
    if (x[i] > upper) {
      x[i] = NA
    }
  }
  x <- x[!is.na(x)]
  return(x) 
}
