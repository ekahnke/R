#1
A <- matrix(data=c(3, 6, 2, 5), nrow=2, ncol=2, byrow=TRUE)    
b <- matrix(data=c(3, 2), nrow=2, ncol=1, byrow=FALSE)
round(solve(A, b), 3)

A <- matrix(data=c(1, -2, 2, -3), nrow=2, ncol=2, byrow=TRUE)    
b <- matrix(data=c(1, 2), nrow=2, ncol=1, byrow=FALSE)
round(solve(A, b), 3)

#2
A <- matrix(data=c(1, -1, 0, 1, 0, 1, 1, -1, -2), nrow=3, ncol=3, byrow=TRUE)    
b <- matrix(data=c(3, 6, 0), nrow=3, ncol=1, byrow=FALSE)
round(solve(A, b), 3)

A <- matrix(data=c(1, 1, 1, 2, 1, 0, 4, 3, 3), nrow=3, ncol=3, byrow=TRUE)    
b <- matrix(data=c(1, 3, 4), nrow=3, ncol=1, byrow=FALSE)
round(solve(A, b), 3)

#3
A <- matrix(data=c(1, 1, 1, 1, .45, -.55, 0, 0, 0, 1, 0, 0, .14, .2, .2, .1), nrow=4, ncol=4, byrow=TRUE)    
b <- matrix(data=c(250000000, 0, 62500000, 37500000), nrow=4, ncol=1, byrow=FALSE)
round(solve(A, b), 3)

#OR
A <- matrix(data=c(1, 1, 1, 1, .45, -.55, 0, 0, -.25, .75, -.25, -.25, .14, .2, .2, .1), nrow=4, ncol=4, byrow=TRUE)    
b <- matrix(data=c(250000000, 0, 0, 37500000), nrow=4, ncol=1, byrow=FALSE)
round(solve(A, b), 3)

#4
#R not needed

#5
#For c,d
A <- matrix(data=c(1, 2, 1, 3), nrow=2, ncol=2, byrow=TRUE)    
b <- matrix(data=c(2, -4), nrow=2, ncol=1, byrow=FALSE)
round(solve(A, b), 3)

#For a,b
A <- matrix(data=c(1, 0, 0, 1), nrow=2, ncol=2, byrow=TRUE)    
b <- matrix(data=c(-24, -16), nrow=2, ncol=1, byrow=FALSE)
round(solve(A, b), 3)
