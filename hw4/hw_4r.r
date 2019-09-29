init<-rep(0.25,4)
mP<-matrix(0,4,4)
S <-c(1:4)  # state space
mP[1,]<- c(0.62,0.00,0.38,0.00)
mP[2,]<- c(0.55,0.00,0.45,0.00)
mP[3,]<-c(0.00,0.24,0.00,0.76)
mP[4,]<- c(0.00,0.19,0.00,0.81)

rowSums(mP)

n=100
X = c(rep(0,n+1))
X[1]<-sample(S,1, prob=init )
k=1
for (k in 2:(n+1)) {
    X[k]<-sample(S,1, prob=mP[X[k-1],] )
}
X = X[-1]
plot(1:n, X, type="s", col="blue")

empirical_freq = table(X)/n
piHat = empirical_freq

mpiHat = matrix(piHat, ncol=4)
delta = abs(mpiHat - (piHat %*% mP))
deltaSum = sum(delta)


