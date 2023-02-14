getwd()


df <-  read.csv('grants.csv')
str(df)

df$status <- factor(df$status, labels=c('Not funded', "funded"))
str(df)    

#Correlation table
t3 <- table(status=df$status, field=df$field)
t3

# 1 or 2 designates type of axis - 1-row, 2-column
prop.table(t3, 2)

barplot(t3, legend.text = T, beside=T)


mosaicplot(t3)

data <- (HairEyeColor)
data
prop.table(data, 2)
prop.table(data, 2)['Red' ,'Blue', 'Male']
red_men <-  prop.table(HairEyeColor[,, 'Male'], 2)['Red','Blue']
red_men 
prop.table(HairEyeColor[,, 'Male'], 2)
prop.table(data, 2)

barplot(t3)
# if we want to separate bars we should point out arg "beside=True"
barplot(t3, beside=T)


mosaicplot(t3)

dataframe <- as.data.frame(HairEyeColor[ , ,'Female'])

ggplot(dataframe, aes(x=Hair, y=Freq )) +
  geom_bar(stat='identity', data=dataframe, position='jitter') + 
  scale_fill_manual(values=c('brown', 'blue', 'darkgrey', 'darkgreen'))


library("ggplot2")
dataframe <- as.data.frame(HairEyeColor[ , ,'Female'])
obj <- ggplot(data = dataframe, aes(x = Hair, y = Freq, fill=Eye)) + 
  geom_bar(stat="identity", position = position_dodge()) + 
  scale_fill_manual(values=c('brown', 'blue', 'darkgrey', 'darkgreen'))
obj
