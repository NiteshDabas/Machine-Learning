#Fixed-step gradient and conjugate gradient and lm(linear regression) algorithm

getwd()
setwd("Desktop/Nitesh/RWorkingDirectory/")

# read data
datatmp <- read.table("toy_dataset.txt",
                      sep="\t",header=T,
                      blank.lines.skip=T)
print(str(datatmp)) #print structure of data
View(datatmp) #View data

# convert to a matrix
data <- as.matrix(datatmp)

# define dimensions - number of observations
n <- dim(data)[1]
# dimension of the problem
m <- dim(data)[2]-1

# cost function => a = [a0,a1..am] where a0 is the constant
residual <- function(a) {
  residual <- 0
  for (i in 1:n) {
    residual <- residual + 0.5*(data[i,m+1] - a[1] - a[2:(m+1)]%*%data[i,1:m])^2
  }
  return(residual)
}
A <- data
colnames(A) <- NULL
A[,1] <- 1
A[,2:(m+1)] <- data[,1:m]
b <- data[,m+1]

residual2 <- function(a) {
  residual2 <- 0.5*(t(A%*%a-b)%*%(A%*%a-b))
  return(residual2)
}

# gradient of the function
gradient <- function(a) {
  gradient <- t(A)%*%(A%*%a-b)
  return(gradient)
}

# fixed-step gradient algorithm
rho <- 0.02
niter <- 1000
# store iterates
solution <- matrix(data,nrow=(m+1),ncol=(niter+1))
# store values of cost function
values <- matrix(data,nrow=1,ncol=(niter+1))
# initialize
solution[,1] <- 0
values[1] <- residual2(solution[,1])
# iteration loop
for (i in 1:niter) {
  solution[,i+1] = solution[,i]-rho*gradient(solution[,i])
  values[i+1] = residual2(solution[,i+1])
}

plot(1:(niter+1),values)

# built-in linear regression model
L <- lm(y~.,data=datatmp)
summary(L)

# but we want only positive coefficients
# fixed-step gradient algorithm with projection
rho <- 0.02
niter <- 1000
# store iterates
solution <- matrixdata(data,nrow=(m+1),ncol=(niter+1))
# store values of cost function
values <- matrix(data,nrow=1,ncol=(niter+1))
# initialize
solution[,1] <- 0
values[1] <- residual2(solution[,1])
# iteration loop
for (i in 1:niter) {
  temporary = solution[,i]-rho*gradient(solution[,i])
  solution[,i+1] = pmax(temporary,0)
  values[i+1] = residual2(solution[,i+1])
}

# but we want the solution in a much shorter time
# conjugate gradient algorithm
niter <- 20
# store iterates
solution <- matrix(data,nrow=(m+1),ncol=(niter+1))
# store values of cost function
values <- matrix(data,nrow=1,ncol=(niter+1))
# store directions of descent
direction <- matrix(data,nrow=(m+1),ncol=(niter+1))
# initialize
AA <- t(A)%*%A
solution[,1] <- 0
values[1] <- residual2(solution[,1])
direction[,1] <- gradient(solution[,1])
# iteration loop
for (i in 1:niter) {
  rho <- (t(gradient(solution[,i]))%*%direction[,i])/
    (t(AA%*%direction[,i])%*%direction[,i])
  solution[,i+1] = solution[,i]-rho*direction[,i]
  values[i+1] = residual2(solution[,i+1])
  beta = (t(gradient(solution[,i+1]))%*%(AA%*%direction[,i]))/
    (t(AA%*%direction[,i])%*%direction[,i])
  direction[,i+1] = gradient(solution[,i+1])-beta*direction[,i]
}

# eigenvalues of A^T*A
AA <- t(A)%*%A
eigen(AA)

# original dataset
BB <- t(data)%*%data
eigen(BB)