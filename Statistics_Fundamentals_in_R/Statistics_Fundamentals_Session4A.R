#Student distributions
t = seq(-10, 10, 0.05)
y1 = dt(t, 1)   #t-dist with df=1
y5 = dt(t, 5)   #t-dist with df=5
y10 = dt(t, 10) #t-dist with df=10
y50 = dt(t, 50) #t-dist with df=50
ym = max(y1, y5, y10, y50) #setting max y-limit
#plots for t-distributions for different degrees of freedom
plot(t, y1, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='grey')
par(new=TRUE)
plot(t, y5, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='blue')
par(new=TRUE)
plot(t, y10, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='green')
par(new=TRUE)
plot(t, y50, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='red')

#Normal distribution
yn = dnorm(t, 0, 1)
par(new=TRUE)
plot(t, yn, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='black')