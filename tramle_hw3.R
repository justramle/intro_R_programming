#Tram Le
#Homework 3:

      #- - - - - - QUESTION 1 - - - - - #

n = 1000; p = .1
# a. Expected value (mean value)
mean = sum(n*p)
mean

# b. 
# standard deviation 
sd = sqrt(n * p * (1- p))
sd

# c. prob at most 100
a = pbinom(100, n, p)
a

# d. normal approximate of 100 patients
#Z = (100 - mean) / sd
na = pnorm(100, mean, sd, lower.tail = TRUE)
na


      #- - - - - - QUESTION 2 - - - - - # 

n = 5 ; x = 5; p = 0.6
# a. cal binomial prob distribution of exact 5 successes of all red balls
bd = dbinom(x, n, p)
bd

# b. Espected value of trial 
m = n*p
m

# c. standard deviation
sd = sqrt(n*p*(1-p))
sd


      #- - - - - - QUESTION 3 - - - - - #

# percent IQ > 140  x= 140 , mean = 100, sd = 15
mean = 100; sd = 15
# a. probability IQ < 120 cal prob of standard normal distribution
p120 = pnorm(120, mean, sd, lower.tail = TRUE)
p120
# b. probability 110 < IQ < 130
p110 = pnorm(130, mean, sd, lower.tail = TRUE) - pnorm(110, mean, sd, lower.tail = FALSE)
p110

# percent IQ > 140  
p140 = pnorm(140, mean = 100, sd = 15, lower.tail = FALSE)
p140


      #- - - - - - QUESTION 4 - - - - - # 

set.seed(1) 
# random geom with k = 5 and p = .6
n = 100000
countSum = 0

# a. loop though the sum of value that less than 8 then count them
for(i in 1:n){
  if (sum(rgeom(5, .6)) < 8) 
    countSum = countSum + 1
}
print(countSum)

# b. Ratio of the number of sum < 8 / total number of trial n
ratio = countSum / n
ratio
 
# c.Cal negative bionimal distribution
r = 5
p= .6
X <- pnbinom(7 , r ,p)
X

# d. Negative binomial distribution is approximately equal to the sum of geometric distribution


      #- - - - - - QUESTION 5 - - - - - # 

# a.
# population n = 1000000
g = rep(c("Graduate", "Not Graduate"), times = c(320000, 680000))
g

# debug: sum(g == "Graduate") = 320000

# b.Cal sample size 1000
n = 1000
s = sample(g, n)
s

# c. count sample proportion
count = sum(s == "Graduate")
count
# debug: count2 = sum(s == "Not Graduate")
p = count/n
p

# d. Cal the interval: 99% <-> 2.58 z value = .99506
ma = qnorm(.99506)*sqrt(p*(1-p)/n)
ma

#calculate lower and upper bounds of confidence interval
low = p - ma
low

high = p + ma
high

# e.
install.packages("epitools")
library("epitools")
 
# f. 
binom.approx(count, n, conf.level = 0.99)

