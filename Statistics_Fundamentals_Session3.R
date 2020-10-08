x=seq(-5,5,0.01)
x

y=dnorm(x)
y

#as df incrreases to infinity, tdist=normal dist
t=dt(x,0.5)
maxy=max(y,t)
#plot normal distribution in red
plot(x,y,type='l',col='red',xlim=c(-5,5),ylim=c(0,maxy))
par(new=TRUE)
#plot t distribution in blue
plot(x,t,type='l',col='blue',xlim=c(-5,5),ylim=c(0,maxy))

#t distribution wit df=4
t=dt(x,4)
maxy=max(y,t)
#plot normal distribution in red
plot(x,y,type='l',col='red',xlim=c(-5,5),ylim=c(0,maxy))
par(new=TRUE)
#plot t distribution in blue
plot(x,t,type='l',col='blue',xlim=c(-5,5),ylim=c(0,maxy))

#as df incrreases to infinity, tdist=normal dist
t=dt(x,31)
maxy=max(y,t)
#plot normal distribution in red
plot(x,y,type='l',col='red',xlim=c(-5,5),ylim=c(0,maxy))
par(new=TRUE)
#plot t distribution in blue
plot(x,t,type='l',col='blue',xlim=c(-5,5),ylim=c(0,maxy))

#in case of standard normal distribution
pnorm(0.57)-pnorm(-0.28)
#in case not a standard normal distribution - transformation needed
pnorm(1.2,1,2)-pnorm(-0.2,,1,2)
#t-distribution with 4 df(degree of freedom)
pt(0.271,4)-pt(-0.271,4)
qnorm(0.95)

# Degree of Freedom affacts te calculation on fractiles
y2=dt(x,2) #density associated to RV x of t distribution wit df 2
y4=dt(x,4) #df=4
y10=dt(x,10) #df=10
ymax=max(y2,y4,y10) #max value to be able to observe correct trend in plot
plot(x,y2,type='l',col='red',xlim=c(-5,5),ylim=c(0,maxy))
par(new=TRUE)
plot(x,y4,type='l',col='blue',xlim=c(-5,5),ylim=c(0,maxy))
par(new=TRUE)
plot(x,y10,type='l',col='brown',xlim=c(-5,5),ylim=c(0,maxy))

qt(0.9,2)   #fractile of order 90% with df=2
qt(0.9,4)   #fractile of order 90% with df=4
qt(0.9,10)  #fractile of order 90% with df=10

#Simulation - Confidence Interval!!
#To prove: P(mu: CI) = 1-alpha => doesnt mean mu belogs to CI all the time!
data=rnorm(1000,2,1)
#construct the CI for about distribution
lowerbound=mean(data)-1/sqrt(1000)*qnorm(0.975) #1-alpha/2
upperbound=mean(data)+1/sqrt(1000)*qnorm(0.975) #1-alpha/2
CI=c(lowerbound,upperbound)
CI
#please note mu is inside the CI
#but there will be cases when lowerbound is above mu=2 and 2(mu) is not inside the CI
#We are not sure to have mu inside CI

#print 100 instances of CI for data
K=100
M=matrix(data=0,ncol=2,nrow=K)
for(i in 1:K){
  data=rnorm(1000,2,1)
  lowerbound=mean(data)-1/sqrt(1000)*qnorm(0.975) #1-alpha/2
  upperbound=mean(data)+1/sqrt(1000)*qnorm(0.975) #1-alpha/2
  M[i,]=c(lowerbound,upperbound)
}
M #In alpha % of cases mu will be outside CI
#each time lowerbound is greator than 2 replace by 1.
#otherwise each time lowerbound is smaller than 2 replace by 0

#CI Vector: 1: Mu outside the CI. alpha=Level of Significance. 1-alpha=Confidence Level
B=(M[,1]>2)+(M[,2]<2)
B #vector indicating when true value of my parameter is outside the CI
alpha = sum(B)*100/100
alpha

