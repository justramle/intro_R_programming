        #- - - - - - QUESTION 1 - - - - - #
# a. estimate poiny:

# put number of rotten in vector and observ count in vector to find total rotten orange
# then divide for total orange
n = 1000 * 10 

rotOran = c(0:10)
observCount = c(334,369,191,63,22,12,9,0,0,0,0)
p = sum( observCount * rotOran) / n
p

# b. Expected count
# with k = 10, n = 10, p = 0.1142

expCount = dbinom(c(0:10), 10, p) * 1000
expCount


# c. Combine expected count less than 5 then add them together
combineExp = sum(expCount[which(expCount < 5)])
combineExp

# Combine observed count 
combineObs = sum(observCount[6], observCount[7], observCount[8])
combineObs

# d. X^2, Test Statistics after combine on both count
observCount2 = c(334,369,191,63,22,21)
expCount2 = c(297.4, 383.4, 222.4, 76.5,17.3,3.0)

x_square = sum((observCount2 - expCount2)^2 / expCount2)
x_square


# e. alpha = .05. Compute p-value df=6-1-1
x_alpha = qchisq(.05, df = 4, lower.tail = FALSE)
x_alpha

p_value = 1 -  pchisq(x_square, df = 4)
p_value


        #- - - - - - QUESTION 2 - - - - - #

data = matrix(c(15,8,6,12,15,8,8,9,7), nrow = 3, ncol = 3)
data

chi = chisq.test(data)
chi

# a. H0: Number of years of college a person has completed 
#        is independent of location residence
#    Ha: Ha: Number of years of college a person had completed 
#        is dependent of location

# b. Observed matrix
obsMaxtrix = chi$observed
obsMaxtrix

# c. Expected matrix
expMatrix = chi$expected
expMatrix

# d. Compute the ??^2 statistic.
x_square = chisq.test(obsMaxtrix,expMatrix)
x_square

# e. Compute the X^2 with alpha = .05, df = 4
x_alpha = qchisq(.05, df = 4, lower.tail = FALSE)
x_alpha

# f. P-value (x^2 > 3.006)
p_value = 1 -  pchisq(3.006, df = 4)
p_value

# g. g.	Inconclusion, since p-value > 0.05, we fail to reject H0 and conclude that 
#       there is insufficient evidence to support 
#       number of years of college a person has competed 
#       is related to their location



        #- - - - - - QUESTION 3 - - - - - #
# a. Set work directory to folder that contain file
#getwd()
setwd("C:/Users/traml/OneDrive/Documents/R_programming/hw4")

# b. Read data- frame

subjects = read.csv("SOCR-HeightWeight.csv")
subjects

# c. To see first few data
head = head(subjects)
head

# d. Extract variable from data frame
height = subjects$Height
height

weight = subjects$Weight
weight

# e. 
# 95% confidence interval for height

inter_height = t.test(height, conf.level = .95)
inter_height

inter_weight = t.test(weight, conf.level = .99)
inter_weight





