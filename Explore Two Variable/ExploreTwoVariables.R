########################################################
#lesson5
#######################################################c

##scatter plot
qplot(x = age, y = friend_count, data= pf)
#or
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point()

## ggplot syntax

summary(pf$age)
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point()+xlim(13,90)

##overplotting
##coord_trans()
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 0.05, position = position_jitter(h=0))+xlim(13,90) +coord_trans(y='sqrt')

##Alpha and jitter
ggplot(aes(x=age, y=friendships_initiated),data = pf)+geom_point()+
  geom_jitter(alpha=1/10)
# or
ggplot(aes(x=age, y=friendships_initiated),data = pf)+geom_point(alpha =1/10,position = 'jitter')
# transform y-axis
ggplot(aes(x=age, y=friendships_initiated),data = pf)+geom_point(alpha =1/10,position = position_jitter(h=0))+
  coord_trans(y='sqrt')

##conditional means

#install.packages('dplyr')
library(dplyr)
age_groups<-group_by(pf, age)
pf.fc_by_age1 <-summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(as.numeric(friend_count)),
                          n = n())
head(pf.fc_by_age1)
pf.fc_by_age1<- arrange(pf.fc_by_age1,age)
head(pf.fc_by_age1)

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age1)+geom_line()
#or

pf.fc_by_age2<-pf %>%
  group_by(age) %>%
  summarise(age_groups,
            friend_count_mean = mean(friend_count), friend_count_median = median(as.numeric(friend_count)),n = n()) %>%
  arrange(age)

head(pf.fc_by_age2,20)


##overlaying summarys with raw data
ggplot(aes(x=age, y=friendships_initiated),data = pf)+
  geom_point(alpha =1/20,position = position_jitter(h=0),color= 'orange')+
  coord_cartesian(xlim=c(13,70),ylim = c(0,1000))+
  geom_line(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9),color = 'blue',linetype =2)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1))+
  geom_line(stat = 'summary', fun.y = median,color = 'blue',linetype =2)

##correaltion
cor.test(pf$age,pf$friend_count,method = 'pearson')
#or
with(pf,cor.test(age,friend_count,method = 'pearson'))

##correaltion on subsets
with(subset(pf,age<=70),cor.test(age,friend_count,method = 'pearson'))

##correalation methods
with(subset(pf,age<=70),cor.test(age,friend_count,method = 'spearman'))

##correalation scatter plot
qplot(x = www_likes_received ,data=pf)+scale_x_continuous()
ggplot(aes(x=www_likes_received, y=likes_received),data = pf)+
  geom_point(alpha =1/20,position = position_jitter(h=0),color= 'orange')+
  coord_cartesian(xlim=c(0,30),ylim = c(0,70))+
  geom_line(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9),color = 'blue',linetype =2)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1))+
  geom_line(stat = 'summary', fun.y = median,color = 'blue',linetype =2)

with(subset(pf,likes_received<=50 & www_likes_received <=25),cor.test(likes_received,www_likes_received,method = 'kendall'))

##strong correlation
ggplot(aes(x = www_likes_received, y = likes_received),data = pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$likes_received,0.95))+
  geom_smooth(method = 'lm', color = 'red')

with(pf,cor.test(www_likes_received,likes_received))

## More Caution with Correaltion
#install.packages('alr3')
library(alr3)
data(Mitchell)

ggplot(aes(y = Temp, x = Month),data = Mitchell)+
  geom_point()
qplot(data = Mitchell,Month,Temp)

##Noisy scatterplots
with(Mitchell, cor.test(Month,Temp))

## Making Sense of Data
ggplot(aes(y = Temp, x = Month),data = Mitchell)+
  geom_point()+
  scale_x_continuous(breaks = seq(0,203,12))

## A new perspective

ggplot(aes(y = Temp, x = Month%%12),data = Mitchell)+
  geom_point()

#install.packages('energy')
library(energy)
x <- seq(0, 4*pi, pi/20)
y <- cos(x)
qplot(x = x, y = y)
dcor.ttest(x, y)

## Understanding Noise : Age to Age month

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age1
)+ geom_line()
head(pf.fc_by_age1,10)
pf.fc_by_age1[17:19,]

pf$age_with_months <- pf$age + (1 - pf$dob_month / 12)
#or
pf$age_with_months <- with(pf, age + (1 - dob_month / 12))

## Age with Months Means

age_groups<-group_by(pf, age_with_months)
pf.fc_by_age_months <-summarise(age_groups,
                                friend_count_mean = mean(friend_count),
                                friend_count_median = median(as.numeric(friend_count)),
                                n = n())
pf.fc_by_age_months<- arrange(pf.fc_by_age_months,age_with_months)


'''
pf.fc_by_age_months<-

with(pf, subset(pf,age_with_months,friend_))
pf$friend_count_mean<-by(pf$friend_count,pf$age_with_months,mean)
# or
aggregate(friend_count~age_with_months,data=pf,'mean')
aggregate(friend_count~age_with_months,data=pf,'medain')[2]
aggregate(age_with_months,by)
'''
## Noise in conditional means

ggplot(data = subset(pf.fc_by_age_months,age_with_months<71),aes(x = age_with_months, y = friend_count_mean))+
  geom_line()+
  scale_x_continuous(breaks = seq(13.16667,113.91667,10))

## smoothing conditional means

p1<-ggplot(aes(x = age, y = friend_count_mean), data = subset(pf.fc_by_age1,age<71)
)+ geom_line()+geom_smooth()

p2<-ggplot(data = subset(pf.fc_by_age_months,age_with_months<71),aes(x = age_with_months, y = friend_count_mean))+
  geom_line()+
  scale_x_continuous(breaks = seq(13.16667,113.91667,10))+
  geom_smooth()

p3 <- ggplot(aes(x = round(age/5)*5,y = friend_count),data = subset(pf,age<71))+
  geom_line(stat = 'summary', fun.y = mean)
library(gridExtra)
grid.arrange(p3,p2,p1,ncol=1)

#####################################################

## price vs x


str(diamonds)

#scatter plot
ggplot(aes(x = x, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = carat, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = cut, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = color, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = clarity, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = depth, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = table, y = price), data = diamonds)+
  geom_point()

## correlations

with(diamonds, cor.test(price , x ))
with(diamonds, cor.test(price , y ))
with(diamonds, cor.test(price , z ))

## price vs depth

ggplot(aes(x = depth, y = price), data = diamonds)+
  geom_point(alpha=1/100)+
  scale_x_continuous(breaks = seq(43,79,2))

## correlation - price and depth

with(diamonds, cor.test(price , depth ))

##price vs carat

ggplot(aes(x = carat, y = price), data = diamonds)+
  geom_point()+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))

## price vs volume(x*y*z) # new variable

diamonds$volume<-with(diamonds,x*y*z)
ggplot(aes(x = volume ,y = price),data = diamonds)+
  geom_point()

with(diamonds, cor.test(price , volume ))
detach('package:plyr', unload=TRUE)
library(plyr)

count(diamonds$volume == 0)

## correlations on subsets

ggplot(aes(x = volume ,y = price),data = subset(diamonds,volume!=0 & volume <=800))+
  geom_point()

with(subset(diamonds,volume!=0 & volume <=800), cor.test(price , volume ))

## adjustments- price vs volume
#http://www.ats.ucla.edu/stat/r/faq/smooths.htm

'''
method
smoothing method (function) to use,
eg. lm, glm, gam, loess, rlm.
For datasets with n < 1000 default is loess.
For datasets with 1000 or more observations defaults to gam.
'''
ggplot(aes(x= volume ,y = price),data = subset(diamonds,volume!=0 & volume <=800))+
  geom_point(alpha = 1/100)+
  stat_smooth(method = 'lm')

## mean price by clarity
library(dplyr)
dia_byclar<- group_by(diamonds,clarity)
diamondsByClarity <-dplyr::summarise(dia_byclar,
                                     mean_price = mean(price),
                                     median_price = median(as.numeric(price)),
                                     min_price = min(price),
                                     max_price = max(price),
                                     n = n())

dia_bycolr<- group_by(diamonds,color)
diamondsByColor <-dplyr::summarise(dia_bycolr,
                                   mean_price = mean(price),
                                   median_price = median(as.numeric(price)),
                                   min_price = min(price),
                                   max_price = max(price),
                                   n = n())
## bar charts of mean price

p1<-ggplot(aes(x = clarity, y = mean_price),data = diamondsByClarity)+
  geom_bar(stat = 'identity') 
# If you want the heights of the bars to represent values in the data, use stat="identity" and map a variable to the y aesthetic.

p2<-ggplot(aes(x = color, y = mean_price),data = diamondsByColor)+
  geom_bar(stat = 'identity')

library(gridExtra)
grid.arrange(p1,p2,ncol=1)
###########################################################
# finished lesson5
###########################################################
