#Q2.170)

#a.)
# The two variables ( 5 fruits / veg per day and smoke everyday ) are inversely 
#related. If one variable increases other variable decreases. 

#b.)
library(readxl)
library(readxl)
value <- read_excel("/Users/shashidharnaidu/Downloads/ex02-170brfss.xls")
value

#c).
library(ggplot2)
library(tidyverse)
Graph <- ggplot(value, aes(x = SmokeEveryDay, y = FruitVeg5))+geom_point()
Graph
Graph+ annotate("text", x = 8.5, y = 23.3, label = 'Utah')
# Utah falls on the area among the least smoking everday zone which is towards 
#left with 5 fruits and vegetables consumed daily below the proportion of audlts 
#who eat 5 fruits and vegetables.

#d).
Graph + annotate("text", x = 8.6, y = 27.7, label = "California")
#This implies that California is the least smoking state with 5 fruits and 
#vegetables per day which is very close to the mean value. The 5 fruits and 
#vegetables per day falls above the association line. The number of adults who 
#consume fruits and vegetables is higher than expected.

#e).
# The states with vegetables and fruits being at normal spending capacity is 
#more are California and New York. The people in these states are healthy as 
#they are able to afford Fruits and vegetables and do not prefer to smoke.
#Kentucky and West Virginia will forllow reverse pattern.
Graph + annotate("text",x = 12.1, y = 26.8, label = "New York")+ 
  annotate("text", x = 23.8, y = 25.1, label = "West Virginia")+ 
  annotate("text", x = 23.6, y = 21.1, label = "Kentucky")


#Q7.135
file1 <- read_excel("/Users/shashidharnaidu/Downloads/ex07-135compete.xls")
file1
# Standard error = sd/n^0.5
# Margin of error = T +/- Standard error
#n = 25
a <- mean(file1$pct)
a
b <- sd(file1$pct)
b
n <- 25

Standard_error <- b/(n^0.5)
Standard_error
#Standard error = 6.53
# degree of freedom (n-1) = 24
# At Confidence Interval 95%, t* is 2.064.

t_stat <- 2.064
# Confidence level at 95% for the data set is 64.27102% - 91.2489%. 

upper.limit <- a+(t_stat*Standard_error)
upper.limit
lower.limit <- a-(t_stat*Standard_error)
lower.limit
#The 95% confidence interval is for the given set of data is 64.27102% - 91.24898%

#Q 7.143
#part A

Note <- read_excel("/Users/shashidharnaidu/Downloads/ex07-143read.xls")
Basal <- Note[which(Note$group == 'B'),]
DRTA <- Note[which(Note$group == 'D'),]
STRAT <- Note[which(Note$group == 'S'),]
t.test(x = DRTA$post3, y = Basal$post3)

#Mean reading of DRTA method is higher than Basal method. P value is derived at 
#0.006615 is after running T-test which is significant at 0.05 level. 

# part B
t.test(x = STRAT$post3, y = Basal$post3)
# Mean reading of STRAT method is higher than Basal method.P value is derived at
#0.06743 is after running T-test which is significant at 0.05 level.

