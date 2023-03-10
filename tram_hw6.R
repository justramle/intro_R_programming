library(tidyverse)
library(palmerpenguins)
library(dplyr)
library(skimr)
library(caret)

data(penguins)
head(penguins)

#- - - - - - QUESTION 1 - - - - - #
#setwd("C:/Users/traml/OneDrive/Documents/R_programming/hw6")
#penguins = read.csv("penguins.csv")
#penguins

# use skim to summarize whole dataset dand summary all variable in dataset
#skim(penguins)
summary = summary(penguins)
summary
table(penguins$species)


#set.seed(1234)
# a. split data into training/testing data (80/20 splits)
#split = sample(c(rep(0,0.8*nrow(penguins)),rep(1,0.2* nrow(penguins))))
split = sample(1:nrow(penguins),0.8*nrow(penguins))
split

#select all these row
train_pen = penguins[split,]
train_pen

#select all but these row
test_pen = penguins[-split,]
test_pen

dim(penguins)
dim(train_pen)
dim(test_pen)


# b. training model using logistic regrsio
logistic_model = glm(species~bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, family = "binomial", data= train_pen)
logistic_model

summary(logistic_model)

# c. Using model to predict of the testing data
predict_test = predict(logistic_model,test_pen,type = "response")
predict_test


# d. Cal confusion matrix (0= false, 1= True)
predict_tes = ifelse(predict_test >.5,1,0)
predict_tes


confusion_matrix = table(test_pen$species,predict_tes)
confusion_matrix


# e. Calculate TP=true positive, TN=true negative, FP=false postitive, FN=false negative

#1st row 1col: actual and predict shoule be the same
tp = confusion_matrix[1]
tp

# sum of value col except tp
fp = sum(confusion_matrix[2],confusion_matrix[3])
fp

# sum of value row excepts tp
fn = confusion_matrix[4]
fn

# sum of all col and row except values we calc
tn = sum(confusion_matrix[5],confusion_matrix[6])
tn


# f. Cal accuracy
precision = tp/sum(tp,fp)
precision

recall = tp/sum(tp,fn)
recall


accuracy = sum(tp,tn)/sum(tp,tn,fp,fn)
accuracy

err = 1 - accuracy
err



#- - - - - - QUESTION 2 - - - - - #
library(e1071)
library(caTools)
library(class) 

data(penguins)


#split data into train and test
split = sample(1:nrow(penguins),0.8*nrow(penguins))
split

#train and test on whole dataset
train_pen = penguins[split,]
train_pen
test_pen = penguins[-split,]
test_pen

dim(train_pen)
dim(test_pen)

# category species on row 1
pen_train_category = penguins[split,1]
pen_train_category
pen_test_category = penguins[-split,1]
pen_test_category

# Fitting KNN Model 
# to training dataset K =1
nn1 =knn(train_pen[,3:6],test_pen[,3:6],cl=pen_train_category,k=1,prob=TRUE)
nn1

tab1 = table(nn1,pen_test_category)
tab1

accuracy = function(x){sum(diag(x))/sum(rowSums(x))*100}
accuracy(tab1)


# K = 5
nn5 =knn(train_pen[,3:6],test_pen[,3:6],cl=pen_train_category,k=5,prob=TRUE)
nn5

tab5 = table(nn5,pen_test_category)
tab5

accuracy = function(x){sum(diag(x))/sum(rowSums(x))*100}
accuracy(tab5)

# K = 11
nn11 =knn(train_pen[,3:6],test_pen[,3:6],cl=pen_train_category,k=11,prob=TRUE)
nn11

tab11 = table(nn11,pen_test_category)
tab11

accuracy = function(x){sum(diag(x))/sum(rowSums(x))*100}
accuracy(tab11)





