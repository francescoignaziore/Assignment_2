#1.a

A <- matrix(c(1, 5, -2, 1, 2, -1, 3, 6, -3), ncol = 3)
A %*% A %*% A

#1.b

A[,3] <- A[,2] + A[,3]

#2
B <- matrix(rep(c(10, -10, 10), c(15, 15, 15)), ncol= 3)

crossprod(B)
#3
matE <- matrix(0, 6, 6)
matE[abs(col(matE) - row(matE)) == 1] <- 1

#4
outer(0:4, 0:4, "+")

#5.a
outer(0:4, 0:4, "+") %% 5

#5.b
outer(0:9, 0:9, "+") %% 10

#5.c
outer(0:8, 0:8, "-") %% 9

#6
A  <- matrix(0, 5, 5)
An <- matrix(abs(row(A) - col(A)) + 1, 5, 5)
b  <- c(7, -1, -3, 5, 17)
solve(An, b)

#7.a
set.seed(75)
aMat <- matrix(sample(10, size = 60, replace = T), nr = 6)

apply(aMat, 1, function(x){sum(x > 4)})

#7.b

which( apply(aMat, 1, function(x){sum(x == 7) == 2}) ) 

#or

MaT <- apply(aMat, 1, function(x){ x %% 7 == 0 })
Sev <- apply(MaT, 2, function(x){sum(x)})
(1 : length(Sev))[Sev == 2 ]

#7.c

aMatsum <- colSums(aMat)
which(outer(aMatsum, aMatsum, "+") > 75, arr.ind = T)

#8.a
sum((1 : 20) ^ 4) * sum(1 / (4 : 8))

#8.b

sum((1 : 20) ^ 4 / (3 + outer(1 : 20, 1 : 5, "*")))

#8.c

sum(outer(1 : 10, 1 : 10, function(i, k){ (i >= k) * i ^ 4/(3 + i * k)}))

