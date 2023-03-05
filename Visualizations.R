data(diamonds)
diamonds


# if I need quick analysis:
qplot(x=price, data=diamonds)

plot_1 <- qplot(x=price,
                y=carat, 
                color=color,
                data=diamonds)
str(plot_1)

qplot(x=cut, y=carat, color=color, data=diamonds)

qplot(diamonds$carat)
depth_hist <- qplot(diamonds$depth)

qplot(x=carat, y=price, color=clarity, data=diamonds)

qplot(x=color, y=price, data=diamonds, geom='violin')


ggplot(diamonds, aes(x=price, y=carat))+
  geom_point()+
  geom_smooth()

ggplot(diamonds)+
  geom_point(aes(x=price, y=carat))+
  geom_smooth(aes(x=price, y=carat))

ggplot(diamonds, aes(x=price, y=carat, color=cut))+
  geom_point()+
  geom_smooth()

ggplot(diamonds, aes(x=price, y=carat, color=cut))+
  geom_smooth()


ggplot(diamonds, aes(x=price, y=carat))+
  geom_point(aes(color=cut), size=.5)+
  geom_smooth(color='red')


data("airquality")
glimpse(airquality)
gr_mean <- group_by(airquality, Month)
mean_temp <- summarise(gr_mean, 
                       mean_t = mean(Temp),
                       mean_w = mean(Wind))
 
ggplot(mean_temp, aes(x=Month, y=mean_t))+
  geom_line()+
  geom_point(aes(size=mean_w, color='red'))+
  geom_hline(yintercept = 75, linetype='dotted', color='blue')
 # Errorbar


gr_mtcars <- group_by(mtcars, am, vs)
df_s <- summarise(gr_mtcars, mean=mean(mpg),
          y_max = mean+1.96*sd(mpg)/sqrt(length(mean)),
          y_min = mean-1.96*sd(mpg)/sqrt(length(mean)))

ggplot(df_s, aes(x=factor(am), y=mean, col=factor(vs), group=factor(vs)))+
  geom_errorbar(aes(ymin=y_min, ymax=y_max, width=0.2))+
  geom_point()+
  geom_line()

mean_cl_normal(mtcars$mpg)
#         y     ymin     ymax
#     20.09062 17.91768 22.26357

ggplot(mtcars, aes(x=factor(am), y=mpg, col=factor(vs), group=factor(vs)))+
  stat_summary(fun.data = mean_cl_normal, geom='errorbar', width=0.2, position=position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_normal, geom='point', position=position_dodge(0.2)) + 
  stat_summary(fun.data = mean_cl_normal, geom='line', position=position_dodge(0.2))
  #point takes first number in row y, ymin, ymax - explicitly mean (y) 


ggplot(mtcars, aes(x=factor(am), y=mpg))+
  geom_violin()+
  geom_boxplot(width=0.2)


sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv", stringsAsFactors = T)
glimpse(sales)


ggplot(sales, aes(x=income, y=sale))+
  geom_point(aes(color=shop))+
  geom_smooth()


ggplot(sales, aes(x=shop, y=income, color=factor(season)))+
  stat_summary(fun.data = mean_cl_boot, position=position_dodge(0.2))

ggplot(sales, aes(x=date, y=sale, color=shop))+
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', position=position_dodge(0.2)) + # добавим стандартную ошибку
  stat_summary(fun.data = mean_cl_boot, geom='point', position=position_dodge(0.2)) + # добавим точки
  stat_summary(fun.data = mean_cl_boot, geom='line', position=position_dodge(0.2))


# FACET

glimpse(diamonds)


ggplot(diamonds, aes(carat, fill=cut))+
  geom_density(alpha=.3)

ggplot(diamonds, aes(carat))+
  geom_density()+
  facet_grid(factor(color)~factor(cut))


ggplot(diamonds, aes(carat))+
  geom_density()+
  facet_grid(.~factor(cut))


ggplot(diamonds, aes(carat))+
  geom_density()+
  facet_grid(factor(cut)~.)

