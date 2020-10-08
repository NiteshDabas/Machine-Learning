#rnorm is to simulate observations according to gausiian/normal distribution
#10 is number of observations
#0.5 is the expectation
#4 is standard deviation
A=rnorm(10,0.5,4) #A = vector of 10 values(observations) from 10 different RVs
mean(A)
A=rnorm(10,0.5,4)
mean(A)
A=rnorm(10,0.5,4)
mean(A)
#huge fluctiuations in mean with each sample but as we increase 
#number of observations the fluctaions are less and less!
B=rnorm(10000,0.5,5)
mean(B)
B=rnorm(10000,0.5,5)
mean(B)
B=rnorm(10000,0.5,5)
mean(B)
B=rnorm(10000,0.5,5)
mean(B)

Am=matrix(rnorm(500,0.5,4), ncol=10)
Am
meanA=apply(Am, 1, mean) #we apply compute mean for each row of Am object

#setting seed - with fixed seed exactly te same simulated observations
set.seed(1000)
A=rnorm(10,0,1)
A
#without seed not same observations
A=rnorm(10,0,1)
A
#with seed its the same observations
set.seed(1203)
A=rnorm(10,0,1)
A
meanA=apply(Am, 1, mean)
meanA
boxplot(meanA)

Bm=matrix(rnorm(500000,0.5,4), ncol=10000)
Bm
meanB=apply(Bm, 1, mean)
boxplot(meanB)

#two boxplots in same window
par(mfrow=c(1,2))
boxplot(meanA)
boxplot(meanB)

mean(meanA)
mean(meanB)

