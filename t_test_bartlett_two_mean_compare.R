data('iris')
df <-  subset(iris, Species!='setosa')
test1 <- t.test(Sepal.Length ~ Species, df)
ggplot(df, aes(x=Species, y=Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom='errorbar')+
  stat_summary(fun.y = mean, geom='point')

df1 <-  read.table('dataset_11504_15 .txt')
group1 <- df1$V1[df1$V2 == 1]
group2 <- df1$V1[df1$V2 == 2]

bartlett.test(V1~V2, df1)

t.test(V1 ~ V2, df1, var.equal = TRUE)




