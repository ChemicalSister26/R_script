

data('mtcars')
str(mtcars)

mtcars$vs <- as.factor(mtcars$vs <-  c('opp', 'vobr'))
mtcars$am <- as.factor(mtcars$am <-  c('auto', 'manual'))
mtcars$cyl <- as.factor(mtcars$cyl)
data1 <- subset(mtcars, select=c('cyl', 'am'))


smart_test <-  function(x){
    subset_table <- table(x)
    if (min(subset_table)<5) {
      fisher_test <- fisher.test(subset_table)
      return(fisher_test$p.value)
}
    else {
      chi_test <- chisq.test(subset_table)
      return(c(chi_test$statistic, chi_test$parameter, chi_test$p.value))
    }}
smart_test(mtcars[,c("am", "vs")])
smart_test(mtcars[1:20,c("am", "vs")])
