#library(ggplot2)
#library(dplyr)
#library(broom)
#library(ggpubr)

#- - - - - - QUESTION 1 - - - - - #
#Load data file and store it into my_data variable
my_data = read.csv(file.choose())
my_data

#Give summary of data
summary(my_data)

#Use ANOVA to determine fighter's stance has any effect on tko_win_ratio
fighter_aov = aov(my_data$tko_win_ratio ~ my_data$stance)
fighter_aov
summary(fighter_aov)


#- - - - - - QUESTION 2 - - - - - #
#a. Load data file and store it into Diet.data variable
Diet.data = read.csv(file.choose())
Diet.data

#b. Give summary of data
summary(Diet.data)
#attach data so it will keep using diet dataset
attach(Diet.data)

#c. Add new varibale name weightloss to dataset and calculate the dif betwwen
#   weight before and weight diet after 6 weeks
Diet.data$weightlost = weight - weight6weeks 
weightlost

#d. Use ANOVA with two factors Diet and Gender then call it model
model = aov(Diet.data$weightlost~factor(Diet.data$gender)*factor(Diet.data$Diet))
#model = aov(weightlost~factor(Diet)*factor(gender), data = Diet.data)
model

#e. Use leveneTest for equality of variances
leveneTest(Diet.data$weightlost~factor(Diet.data$Diet)*factor(Diet.data$gender))
summary(model)

#f. Use Tukey Honest Significant Differences to see any dif in means
TukeyHSD(model)

