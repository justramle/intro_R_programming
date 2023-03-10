#Tram Le
# Homework1: Vector & Matrix

## 1. Create a vector a = (5, 10, 15, 20, . . . , 200).
a <- seq(5, 200, by = 5)
a
# How many elements are there in a?
length(a)

# What are the 10th, 19th and 22nd elements of a?
a[10] 
a[19] 
a[22]

# Create a vector which is obtained by multiplying each element of a by 0.1.
b <- a * .1
b

# Create a vector which consists of odd numbers of a.
c <- a[which(a %% 2 != 0)]
c

# Sum over all even elements of a.
sum(a[which(a %% 2 == 0)])

# Create a vector which consists of elements of a divisible by 3.
d <- a[which(a %% 3 == 0)]
d

## 2. Create a matrix A (3 rows and 3 columns) by ordering the vector (5, 6, 7, . . . , 13) by rows.
A <- matrix(5:13, byrow = TRUE, nrow = 3)
A

# Find the second row of A.
A[2,]

# Find third column of A.
A[,3]

# Find the transpose of A.
t(A)

# Create a diagonal matrix B consisting diagonal elements of A.
B <- diag(A)
B

# Find the inverse of B.
B <- solve(A)
B

# Create a matrix by adding one more column with elements (2, 1, 5) to A.
A <- cbind(A, c(2, 1, 5))
A

# Create a matrix by adding one more row with elements (0.3, ???1.1, 3.5) to A.
A <- matrix(5:13, byrow = TRUE, nrow = 3)
A <- rbind(A, c(0.3, -1.1, 3.5))
A
