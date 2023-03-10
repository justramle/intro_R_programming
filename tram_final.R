
#- - - - - - QUESTION 1 - - - - - #

# 1. Load built in dataset and give it a summary
data(swiss)
head(swiss)

summary(swiss)
# see name of all variables
names(swiss)
cor(swiss)

# H0: b1 = b2 = b3 = b4 = b5 = 0
# Ha: those are not equal to 0

# a. Fit linear regression model to education
edu_model1 = lm(Education~Fertility + Agriculture + 
                 Examination + Catholic + Infant.Mortality,
               data = swiss)
edu_model1

#significant for regression
summary(edu_model1)

# p value of Fertility, Agriculture, Catholic < .05
# these variables are significant
# Examination and Infant.Mortality are not significant

# b. Use significant variables to construct multi gression
edu_model2 = lm(Education~Fertility + Agriculture + 
                Catholic, data = swiss)
edu_model2
summary(edu_model2)

# c.
# estimate education by pluging X value to each observation to see the draffee

edu_model3 = 53.8505 + (-0.48883)*60.00 + (-0.23799)*55.00 +
                    (0.08440)*4.15
edu_model3

# the estimate education level for draftee is 11.78151. This mean average a drafee can 
# expect to have education level of over 11 years

# after took  Examination and Infant.Mortality out of the linear regression, t-value increased
# higher F-statistic and smaller p-value, model looks better 




#- - - - - - QUESTION 2 - - - - - #
library(car)
library(rstatix)

data("InsectSprays")
head(InsectSprays, 10)

# H0: both variances are equal
# Ha: At least one of them differ

# a. Levene's Test
leveneTest(count ~ spray, data = InsectSprays)

# b. use oneway.test to see if spray affect on count or not

oneway.test(count ~ spray, data = InsectSprays)

# c.
# one-way ANOVA model on Tukey Test
model_insect = aov(count~spray, data = InsectSprays)
model_insect
TukeyHSD(model_insect)

# 
games_howell_test(count ~ spray, data = InsectSprays)



#- - - - - - QUESTION 3 - - - - - #
library(caTools)
library(tidyverse)

set.seed(123)

my_data = read.csv(file.choose("wine.csv"))
my_data

summary(my_data)

# split class of wine data using 80/20 split
split_wine = sample.split(wine$Wine, SplitRatio = 0.8)
split_wine


train_wine = wine[split_wine,]
train_wine
test_wine = wine[-split_wine,]
test_wine

# build training model using logistic regression based on 13 variables
log_model = glm(wine$Wine ~., data = train_wine, family = "binomial")

summary(log_model)

# prefict on new data test_wine
predict_wine = predict(log_model, type = "response", newdata = test_wine)

#make confusion matrix
confu_matrix = table(test_wine$Wine, predict_wine > .5)

# after result we get accuracy
accuracy1 = (106 + 59)/(106 + 59 + 8 + 4)
accuracy

# using KNN on same split and data:
library(class)

dim(test_wine)

# K = 1

knn_model = knn(train_wine[,-1], test_wine[,-1], cl = train_wine$Wine, k = 1)
prediction = knn_model
accuracy2 = sum(prediction == test_wine$Wine) / nrow(test_wine)
accuracy2

# K = 3
knn_model2 = knn(train_wine[,-1], test_wine[,-1], cl = train_wine$Wine, k = 3)
accuracy3 = sum(knn_model2 == test_wine$Wine) / nrow(test_wine)
accuracy3


# K = 10
knn_model2 = knn(train_wine[,-1], test_wine[,-1], cl = train_wine$Wine, k = 10)
accuracy3 = sum(knn_model2 == test_wine$Wine) / nrow(test_wine)
accuracy3


# both model have accuracy aound 95% but when I decrease the K of Knn
# the accuracy also decrease dramicly to 75%
# So the logistic regression model is still the best option

#- - - - - - QUESTION 4 - - - - - #
library (tidyverse)
#library (collection)

set.seed (123)

my_data = read.csv(file.choose("wine.csv"))
my_data

# remove class of wine
wine_cluster = wine[,-1]
wine_cluster

# read collection kmeans with k = 3
collection = kmeans(wine_cluster, 3)
collection

# add col back to data
wine$cluster = collection$cluster
wine$cluster


