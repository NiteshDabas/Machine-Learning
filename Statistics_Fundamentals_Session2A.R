A=rexp(1000,0.4)
A
summary(A)
plot(A)
#Barplot-Discrete. Histogram-Continuous
hist(A)   #Not Correct
hist(A, freq = FALSE)

#Why density on y axis and not frequency
B= runif(1000,0,4)
B
H=hist(B, breaks = c(0,1,2,3,4))
H
names(H)
H$breaks #0 1 3 4
H$counts #[1] 250 512 238
H=hist(B, breaks = c(0,1,3,4))
H$counts
hist(A, freq = FALSE)

x= seq(-10,10,0.01)
plot(x,dexp(x,0.4),type = 'l', col='red')
plot(x,dnorm(x,2,0.5),type = 'l', col='red')
plot(x,dunif(x,-3.5),type = 'l', col='red')

H = hist(A, freq = FALSE)
H$density

C=rexp(100000, 0.4) #true value 0.4
par(mfrow=c(2,2))
#hist(C, breaks = 5, freq = FALSE)
hist(C, breaks = 20, freq = FALSE)
hist(C, breaks = 50, freq = FALSE)
hist(C, breaks = 100, freq = FALSE)
hist(C, breaks = 500, freq = FALSE)
#extimated value = 1/mean(A) = 0.3985
EstimatedVal <- 1/mean(A)
EstimatedVal
#Why its intesting to have big dataset?
#Trajectory assoxciated to different saple size
length(A) #A is dataset of size 1000
sumx=cumsum(A) #cumulative sum function  wioth o/p x1, x1+x2, x1+x2+x3
A[1:5]
sumx[1:5]
barx=sumx/1:1000
barx[1:5]
estimalambda = 1/barx
estimalambda[1:5]
max(estimalambda)
plot(1:1000, estimalambda, type = 'l', col='red', xlim=c(0,1001), ylim=c(0,0.5))
par(new=TRUE)
plot(c(0,1000),c(0.4,0.4), type = 'l', col='green', xlim=c(0,1001), ylim = c(0,0.5))

#par(mfrow=c(1,1))
D=matrix(rexp(50*1000),ncol=1000)
meanD=apply(D,1,mean)
estimalambda=1/meanD
boxplot(meanD)
#fluctuation increases as datasize decreases
D=matrix(rexp(50*100),ncol=100)
meanD=apply(D,1,mean)
estimalambda=1/meanD
boxplot(meanD)
#fluctuation increases as datasize decreases
D=matrix(rexp(50*50),ncol=50)
meanD=apply(D,1,mean)
estimalambda=1/meanD
boxplot(meanD)
#fluctuation decreases as datasize increases
D=matrix(rexp(50*100000),ncol=100000)
meanD=apply(D,1,mean)
estimalambda=1/meanD
boxplot(meanD)
