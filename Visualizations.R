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


glimpse(mtcars)
mtcars$vs <- factor(mtcars$vs, labels=c("S", "V"))
mtcars$am <- factor(mtcars$am, labels=c("A", "M"))

ggplot(mtcars, aes(x=hp, y=mpg))+
  geom_point(aes(color=factor(cyl)))+
  facet_grid(vs~.)+
  geom_smooth()


# FACET WRAP
ggplot(diamonds, aes(carat))+
  geom_density()+
  facet_wrap(~ factor(cut)+factor(color))


ggplot(mtcars, aes(x=mpg))+
  geom_dotplot()+
  facet_grid(am~vs)


ggplot(iris, aes(x=Sepal.Length))+
  geom_density()+
  facet_wrap(~Species)


ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Species)


movie <- read.csv('myMovieData.csv', stringsAsFactors = T)
glimpse(movie)


ggplot(movie, aes(x=Type, y=Budget))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(mtcars, aes(x=hp, y=mpg, col=factor(am)))+
  geom_point()+
  scale_x_continuous(name='new_name',
                     breaks=c(180, 220),
                     limits=c(10, 350))


ggplot(mtcars, aes(x=hp, y=mpg, col=factor(am)))+
  geom_point()+
  scale_x_continuous(expand=c(1,3))+
  scale_color_discrete(name="Gear",
                       labels=c("Auto", "Manual"))+
  scale_color_manual(values=c('green', 'yellow'))


ggplot(mtcars, aes(hp, fill=factor(am)))+
  geom_density(alpha=0.4)+
  scale_fill_discrete(name='gear')+
  scale_fill_manual(values = c('brown', 'lightblue'))


ggplot(mtcars, aes(x=hp, y=mpg, size=disp))+
  geom_point()+
  scale_size_continuous(name='what is this?')


ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species))+
  geom_point()+
  geom_smooth(method='lm')+
  xlab("Длина чашелистика")+
  ylab("Длина лепестка")+
  scale_color_discrete(name='Вид цветка', 
                       labels=c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский"))+
  scale_x_continuous(breaks = seq(4,8,1),
                     limits=c(4,8))+
  scale_y_continuous(breaks=seq(1, 7, 1))

ggplot(mtcars, aes(factor(am), hp, fill=factor(cyl)))+
  geom_boxplot()+
  scale_fill_brewer(type='qual', palette = 6)+
  theme_bw()+
  theme(text=element_text(size=14), 
        axis.line.x = element_line(color='red', linewidth =2),
        axis.line.y = element_line(color='red', linewidth =2))

ggplot(mtcars, aes(factor(am), hp, fill=factor(cyl)))+
  geom_boxplot()+
  theme_solarized()


vis <- read.csv('example_data.csv', stringsAsFactors = T)

glimpse(vis)

ggplot(vis, aes(x=date, 
                y=percent, 
                col=system,
                group=system))+
  geom_line()+
  geom_point(shape=21, size=3, fill='black')+
  geom_point(shape=21, size=5.5)+
  geom_point(shape=21, size=4)+ #we make the thickness of point lines more impressive
  geom_vline(xintercept = 7.5, color='white')+
  scale_y_continuous(breaks=c(0.0, 0.04, 0.08, 0.11, 0.15), 
                     limits = c(0, 0.15),
                     labels=scales::percent)+
  scale_color_manual(values = c('orangered1', 'red', 'cyan', 'yellow1', 'springgreen2'))+
  xlab('')+
  ylab('')+
  ggtitle('Popularity of Ubuntu family')+
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = 'top', 
        plot.background = element_rect(color='black', fill='black'),
        text = element_text(color='white'),
        panel.background = element_rect(fill='black'),
        legend.background = element_rect(color='black', fill='black'),
        panel.grid.major.y = element_line(color='grey', linetype = 'dotted'),
        axis.text.x = element_text(face='bold', size=16, color='white'),
        axis.text.y = element_text(face='bold', size=16, color='white'),
        legend.text = element_text(face='bold', size=16, color='white'),
        title = element_text(face='bold', size=16, color='white'))

