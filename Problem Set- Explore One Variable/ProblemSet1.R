########################################################
#lesson4
#######################################################

library(ggplot2)
library(dplyr)
library(scales)
library(xlsx)
library(reshape2)
library(lubridate)
library(ggthemes)
library(gridExtra)

####Q3.1

# a) Load the 'diamonds' data set in R Studio. 
# How many observations are in the data set?

nrow(diamonds)

## [1] 53940

# b) How many variables are in the data set?

ncol(diamonds)

## [1] 10

# c) How many ordered factors are in the set?

# str(diamonds)
# 3

# d) What letter represents the best color for a diamonds?

levels(diamonds$color)

## [1] "D" "E" "F" "G" "H" "I" "J"

# help(diamonds)
# D

####Q3.2

# Create a histogram of the price of
# all the diamonds in the diamond data set.

ggplot(diamonds, aes(x = price)) + 
geom_histogram(color = "black", fill = "DarkOrange", binwidth = 500) + 
scale_x_continuous(labels = dollar, breaks = seq(0, 20000, 1000)) + 
theme(axis.text.x = element_text(angle = 90)) + 
xlab("Price") + ylab("Count")


####Q3.3

#Describe the shape and center of the price distribution. Include summary statistics like the mean and median.

summary(diamonds$price)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     326     950    2400    3930    5320   18800

# 
# The distribution is right-skewed with small amounts of very large prices driving up the mean, while the median remains a more robust measure of the center of the distribution.

####Q3.4

# a) How many diamonds cost less than $500?
summary(diamonds$price < 500) # 1729


# b) How many diamonds cost less than $250?
summary(diamonds$price < 250) # 0

# c) How many diamonds cost more than $15,000?
summary(diamonds$price >= 15000) # 1656


####Q3.5

# Explore the largest peak in the
# price histogram you created earlier.

# Try limiting the x-axis, altering the bin width,
# and setting different breaks on the x-axis.

ggplot(diamonds, aes(x = price)) + 
geom_histogram(color = "black", fill = "DarkOrange", binwidth = 25) + 
scale_x_continuous(labels = dollar, breaks = seq(0, 2000, 100)) + 
theme(axis.text.x = element_text(angle = 90)) + 
coord_cartesian(c(0,2000)) +
xlab("Price") + ylab("Count")

qplot(price, data = diamonds, binwidth = 25,
    color = I('black'), fill = I('#099DD9')) +
scale_x_continuous(breaks = seq(300, 2000, 100)) +
theme(axis.text.x = element_text(angle = 90)) +
coord_cartesian(c(0,2000)) +
xlab("Price") + ylab("Count")

####Q3.6

# Break out the histogram of diamond prices by cut.

# You should have five histograms in separate
# panels on your resulting plot.

ggplot(diamonds, aes(x = price)) + 
geom_histogram(color = "black", fill = "DarkOrange", binwidth = 25) + 
scale_x_continuous(labels = dollar, breaks = seq(0, 4000, 100)) + 
theme(axis.text.x = element_text(angle = 90)) + 
coord_cartesian(c(0,4000)) +
facet_grid(cut~.) + 
xlab("Price") + ylab("Count")


qplot(price, data = diamonds, binwidth = 25,
    color = I('black'), fill = I('#099DD9')) +
scale_x_continuous(breaks = seq(0, 4000, 100)) +
theme(axis.text.x = element_text(angle = 90)) +
coord_cartesian(c(0, 4000)) +
xlab("Price") + ylab("Count") +
facet_grid(cut~.)

####Q3.7

# a) Which cut has the highest priced diamond?
# Premium

by(diamonds$price, diamonds$cut, max)


# b) Which cut has the lowest priced diamond?
by(diamonds$price, diamonds$cut, min)

# Premium & Ideal

# c) Which cut has the lowest median price?
by(diamonds$price, diamonds$cut, median)
# Ideal

####Q3.8

# In the two last exercises, we looked at
# the distribution for diamonds by cut.

# Run the code below in R Studio to generate
# the histogram as a reminder.

# ===============================================================

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free")

# ===============================================================

# In the last exercise, we looked at the summary statistics
# for diamond price by cut. If we look at the output table, the
# the median and quartiles are reasonably close to each other.

# diamonds$cut: Fair
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     337    2050    3282    4359    5206   18570 
# ------------------------------------------------------------------------ 
# diamonds$cut: Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     327    1145    3050    3929    5028   18790 
# ------------------------------------------------------------------------ 
# diamonds$cut: Very Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     336     912    2648    3982    5373   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Premium
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326    1046    3185    4584    6296   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Ideal
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326     878    1810    3458    4678   18810 

# This means the distributions should be somewhat similar,
# but the histograms we created don't show that.

# The 'Fair' and 'Good' diamonds appear to have 
# different distributions compared to the better
# cut diamonds. They seem somewhat uniform
# on the left with long tails on the right.

# Let's look in to this more.

# Look up the documentation for facet_wrap in R Studio.
# Then, scroll back up and add a parameter to facet_wrap so that
# the y-axis in the histograms is not fixed. You want the y-axis to
# be different for each histogram.


####Q3.9

# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.

# Adjust the bin width and transform the scale
# of the x-axis using log10.

# ===========================================================================


ggplot(diamonds, aes(x = price/carat)) + 
geom_histogram(color = "black", fill = "DarkOrange", binwidth = .05) + 
theme(axis.text.x = element_text(angle = 0)) +
scale_x_log10(expression(paste(Log[10], " of Price")),
             breaks = trans_breaks("log10", function(x) 10^x),
             labels = trans_format("log10", math_format(10^.x))) + 
facet_grid(cut~., scale = "free") + ylab("Count")

#### Q3.10

# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.


# There are many more Ideal diamonds than others, but the average price is also the lowest.


ggplot(diamonds, aes(x = clarity, y = price, color = cut)) + 
 geom_boxplot() + 
 facet_grid(color~., margins = TRUE) 

# This boxplot matrix shows the distribution of price across all 3 categorical variables; cut, clarity, and color.

#### Q3.11

# a) What is the price range for the middle 50% of the diamonds with color D?
# b) What is the price range for the middle 50% of diamonds with color J?
# c) What is the IQR for diamonds with the best color?
# d) What is the IQR for the diamonds with the worst color?

by(diamonds$price, diamonds$color, summary)

IQR(subset(diamonds, price <1000)$price)

####Q3.12

# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

ggplot(diamonds, aes(x = color, y = price/carat, fill = color)) +
 geom_boxplot() +
 coord_cartesian(ylim=c(1000, 6000)) +
 scale_y_continuous(labels=dollar) + 
 xlab("Color") + ylab("Price per Carat")



####Q3.13

# Investigate the weight of the diamonds (carat) using a frequency polygon. Use different bin widths to see how the frequency polygon changes. What carat size has a count greater than 2000? Check all that apply.

sizes = c(0.1, 0.3, 0.8, 1.01, 1.6, 2.0, 3.0, 5.0)

summary(diamonds$carat)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.200   0.400   0.700   0.798   1.040   5.010

ggplot(diamonds, aes(x=carat)) + 
  geom_freqpoly(binwidth=0.1, alpha = 0.75) + 
  scale_x_continuous(breaks=sizes, expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  geom_vline(xintercept = c(0.1, 0.8, 1.6, 2.0, 3.0, 5.0), color = "red", linetype="dashed", alpha = 0.75) +
  geom_vline(xintercept = c(0.3, 1.01), color = "forestgreen", linetype = "twodash") +
  geom_hline(yintercept = 2000, color = "brown", linetype="longdash", alpha = 0.5) + 
  xlab("Carat Size") + ylab("Count")

####Q3.14

# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.


library(ggplot2)
library(dplyr)
library(scales)
library(rJava)
library(xlsxjars)
library(xlsx)
library(reshape2)
library(lubridate)
library(ggthemes)
library(gridExtra)

hours <- tbl_df(read.xlsx('/Users/Vineets/Downloads/EDA_Course_Materials/
                          Problem Set #1/indicator_hours per week.xlsx',
                          sheetName = "Data", header = TRUE))
summary(hours)
names(hours)

hours <- hours %>%
  select(-NA.) %>% # Removing NA columns
  rename(Country=Working.hours.per.week) %>%
  filter(Country != "<NA>") # Removing NA rows

hours.long <- melt(hours, id=c("Country"), value.name="Hours", 
                   variable.name="Year")
hours.long <- tbl_df(hours.long)
summary(hours.long)

hours.long <- hours.long %>%
  mutate(Year = as.character(Year), # Converting to character
         Year = substr(Year, 2, 5), # Removing 'X' from data and subsetting the remaining text
         Year = as.numeric(Year))  # Converting to numeric

yearStats <- hours.long %>%
  group_by(Year) %>%
  summarize(median = median(Hours, na.rm =TRUE),
            mean = mean(Hours, na.rm=TRUE),
            lower = min(Hours, na.rm=TRUE),
            upper = max(Hours, na.rm=TRUE),
            se = sd(Hours, na.rm=TRUE)/sqrt(length(Hours)),
            avg_upper = mean + (2.101*se),
            avg_lower = mean - (2.101*se),
            quant.25 = quantile(Hours, na.rm=TRUE, 0.25),
            quant.75 = quantile(Hours, na.rm=TRUE, 0.75))

yearStats <- round(yearStats, 2)

p <- ggplot(yearStats, aes(x=Year, y = median)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(yearStats, mapping=aes(x=Year, ymin=lower, ymax=upper), colour = "wheat2", alpha=1, size=5) + 
  geom_linerange(yearStats, mapping=aes(x=Year, ymin=quant.25, ymax=quant.75), colour = "wheat4", size=5) +
  geom_line(yearStats, mapping=aes(x=Year, y=median, group=1)) +
  geom_vline(xintercept = 1980, colour = "wheat4", linetype=1, size=1) + 
  geom_hline(yintercept=seq(26, 56, 2), color="white", linetype=1)

dottedYears <- seq(1980, 2007, 5)
p <- p + geom_vline(xintercept = dottedYears, color="wheat4", linetype=3, size=0.5)
p <- p + coord_cartesian(ylim = c(26,58))+
  scale_y_continuous(breaks=seq(26, 60, 2)) +
  scale_x_continuous(breaks=seq(1980, 2005, 5), expand=c(0,0) )

p <- p + geom_line(data = subset(hours.long, Country == "United States"), aes(x = Year, y = Hours, group = Country), color ="brown") +
  annotate("segment", x=2000, xend=2002, y=35.5, yend=36, color="brown") +
  annotate("text", x=2003, y=36.3, label="U.S. Hours", size=3.5, color="brown") + 
  annotate("segment", x=2000, xend=2001, y=33.5, yend=32) + 
  annotate("text", x=2002, y=31.7, label="World Medians", size=3.5)


p1 <- p + annotate("text", x=1999.9, y=56, label="Data represents hours worked per week for 52 countries   ", size=3, color="gray30") + 
  annotate("text", x=2000, y=55, label="from 1980 to 2007. Outer lighter bands show the min/max  ", size=3, color="gray30") +
  annotate("text", x=2000, y=54, label="hours for each year, and inner darker bands show the IQR.", size=3, color="gray30") + 
  ggtitle("World's Working Hours") +
  theme(plot.title=element_text(face="bold",hjust=.95,vjust=.8,color="#3C3C3C",size=20)) + 
  annotate("text", x=1994.6, y=57.5, label="Weekly", size=4, fontface="bold")
p1

p2 <- p + coord_cartesian(ylim=c(30, 38)) + 
  ggtitle("Inter-Quartile Range of Weekly Hours Worked Per Year Per Country") +
  theme(plot.title=element_text(face="bold",color="#3C3C3C",size=12)) + 
  geom_text(data = hours.long[hours.long$Country == "United States",], aes( x = Year, y = Hours, color = Country, group = Country, label = round(Hours, 2)), hjust = -.1, vjust = -1.2, size = 2, color = "brown") +
  geom_text( aes(x = Year, y = median, label = median), hjust = -.1, vjust = 1.2, size = 2, color = "black")
p2

p3 <- ggplot(subset(hours.long, Country %in% c("Switzerland", "United States", "Japan", "United Kingdom", "France")),
             aes(x = Country, y = Hours, fill= Country)) + 
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", shape = 5) + 
  ylab("Hours worked per week") + xlab("") + 
  theme(legend.position = "none") + 
  ggtitle("Box plot of Hours worked weekly for 5 countries") + 
  theme(plot.title =element_text(face = "bold", color = "Black", size = 12))
p3

p4 <- ggplot(subset(hours.long, Country %in% c("Switzerland", "United States", "Japan", "United Kingdom", "France")), 
             aes(x = Year, y = Hours)) +
  geom_line(aes(color = Country, group = Country)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(breaks=seq(26, 56, 2)) +
  scale_x_continuous(breaks=seq(1980, 2005, 5), expand=c(0,0) ) +
  ylab("Hours worked per week") + xlab("") + 
  ggtitle("Hours worked weekly from 1980-2007 for 5 countries") + 
  theme(plot.title =element_text(face = "bold", color = "Black", size = 12))
p4






###########################################################
# finished lesson4
###########################################################
