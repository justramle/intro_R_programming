#Tram Le
#Homework 2: 

        #- - - - - - QUESTION 1 - - - - - #

# Set the seed  in the interval 0-1 to reproduce the same output each time execuse
set.seed(1)
# Generate a random 1000 uniform between 0-1 
n = 1000
x <- runif(n,0,1)
x

# a. mean of x 
mean(x)

# b. variance of x 
var(x)

# c. cummulative sum of x 
cs <- cumsum(x)
cs
 
# d. plot 1:n vs cs/(1:n) as type = "l"
#plot(1:n, type = "l")
#plot(cs/1:n, type = "l")

plot(1:n, cs/1:n, type = "l")

# e. horizontal line at y = .5
abline(h = .5)

        #- - - - - - QUESTION 2 - - - - - #

# E(X) by integrate from 0 to 1 of funtion x*dx
f = function(x) {x}
m <- integrate(f,0,1)
m

# Var(X) by integrate from 0 to 1 of funtion (x-1/2)^2 * dx
g = function(x) {(x-1/2)^2}
v <- integrate(g,0,1)
v

        #- - - - - - QUESTION 3 - - - - - #
n = 1000000
x = runif(n,-1,1)

y = runif(n,-1,1)

  #m = x^2 + y^2 < 1
  # Way 2: could using length() with value of m that == TRUE to find how many points that satisfy x^2 + y^2 <1
  #length(m[m == TRUE])

# a. count if value x^2 + y^2 < 1 
count = 0
for (i in 1:n)
{
  if (x[i]^2 + y[i]^2 < 1)
    count = count + 1
}
# total numbers of satisfies x^2 + y^2 <1
print(count)

# Ratio of total # satisfies and total # of points (n)
ratio = count / n
ratio

# 
Area = 4 * ratio
Area

        #- - - - - - QUESTION 4 - - - - - #

n = 100000; count = 0
x = runif(n,-1,1)

y = runif(n,-1,1)

z = runif(n,-1,1)


for (i in 1: n)
{
  if (x[i]^2 + y[i]^4 + z[i]^6 < 1)
  {
    count = count + 1
  }
    
}
print(count)

Ratio = count / n
Ratio

Volume = 8 * Ratio
Volume

        #- - - - - - QUESTION 5 - - - - - #
n = 10000
count = 0

for (i in 1:n)
{
  # create 2 random uniform between 0,1 then sort the order of values
  x = runif(2, 0, 1)
#  print(x)
  x = sort(x)
  # create vector to put 1 and 0 in each row 
  m = c(0,0,0,1)
  # assign each value in the 1st, 2nd column to the 2nd, 3rd value in vector
  m[2] = x[1]
  m[3] = x[2]
  # sort the  difference between pairs in vector (next - previous)
  m2 = sort(diff(m))
  
  #debug line # print(m2)
  
  #after the sort it becomes 3 segments
  #compare the sum of 1st two segments if it greater the 3rd then count it
  if(m2[1] + m2[2] > m2[3])
  {
    count = count + 1
  }
}
print("The probobity is: ")
print(count/n)


