#Diamonds
data(diamonds)
str(diamonds)
table(diamonds$color)

#create histogram of the price of all the diamonds
qplot(data= diamonds, x=price)
summary(diamonds$price)

#less than $500
lessthan500 <- subset(diamonds, price < 500)
str(lessthan500)

#less than $250
lessthan250 <- subset(diamonds, price < 250)
lessthan250
str(lessthan250)

#equal or more than $15,000
morethan15000 <- subset(diamonds, price >= 15000)
morethan15000
str(morethan15000)

#histogram of price by carat
qplot(carat, price, data=diamonds)

#adjust the binwidth
qplot(price, data=diamonds, geom="histogram")
qplot(price, data=diamonds, geom="histogram", binwidth=10)
qplot(price, data=diamonds, geom="histogram", binwidth=5)
qplot(price, data=diamonds, geom="histogram", binwidth=500)
qplot(price, data=diamonds, geom="histogram", binwidth=100)

#zoom
qplot(price, data=diamonds,
      geom="histogram", xlim=c(0, 5000))

#Explore the largest peak in the price histogram
qplot(x = price, data = diamonds)
qplot(x = price, data = diamonds, binwidth=1000)
ggsave('priceHistogram.png')

qplot(price, data=diamonds,
      geom="histogram", xlim=c(0, 5000))
qplot(price, data=diamonds,
      geom="histogram", xlim=c(500, 1500))
qplot(price, data=diamonds,
      geom="histogram", xlim=c(500, 1500), binwidth=1)

str(diamonds)
qplot(cut, data=diamonds)
ggsave('cutHistogram.png')

qplot(y=price, x=cut, data=diamonds, geom="histogram")
ggsave('pricebycutHistogram.png')

qplot(x = price, data = diamonds) + facet_wrap(~cut)

#summary of prices of diamonds by cut
by(diamonds$price, diamonds$cut, summary)
#to solve rounding issue look at max
help("max")
by(diamonds$price, diamonds$cut, max)

#adjust facet wrap for free scales
help("facet_wrap")
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free")

# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get started.

by(diamonds$price, diamonds$carat, summary)
qplot(y=carat, x=price, data=diamonds)
qplot(y=carat, x=price, data=diamonds, xlim=c(500, 1500)
      
      #Price by color box plots
      qplot(x=color, y=price, 
            data = subset(diamonds, !is.na(color)),
            geom = 'boxplot')
      #with limits
      qplot(x=color, y=price, 
            data = subset(diamonds, !is.na(color)),
            geom = 'boxplot', ylim = c(500,5000))
      
      #price range by color range
      by(diamonds$price, diamonds$color, summary)
      
      #Interquartile range (IQR)
      help(IQR)
      IQR(subset(diamonds, price <1000)$price)
      IQR(subset(diamonds, color = "J")$color)
      IQR(subset(diamonds, color = "D")$color)
      
      # Investigate the price per carat of diamonds across
      # the different colors of diamonds using boxplots.
      ppc <- diamonds$price/diamonds$carat
      by(ppc, diamonds$color, summary)
      
      qplot(x=color, y=ppc, 
            data = subset(diamonds, !is.na(color)),
            geom = 'boxplot')
      
      qplot(x=color, y=ppc, 
            data = subset(diamonds, !is.na(color)),
            geom = 'boxplot', ylim = c(1000,4000))
      
      #Frequency polygons
      qplot(x = carat, data = diamonds, geom = "freqpoly")
      qplot(x = carat, data = diamonds, geom = "freqpoly",
            binwidth = 10)
      qplot(x = carat, data = diamonds, geom = "freqpoly",
            binwidth = 1)
      
      library("tidyr","dplyr")
      