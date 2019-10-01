init<-rep(0.25,4)
mP<-matrix(0,4,4)
S <-c(1:4)  # state space
mP[1,]<- c(0.62,0.00,0.38,0.00)
mP[2,]<- c(0.55,0.00,0.45,0.00)
mP[3,]<-c(0.00,0.24,0.00,0.76)
mP[4,]<- c(0.00,0.19,0.00,0.81)

rowSums(mP)

n=1000
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

# f)
mpiHat = matrix(piHat, ncol=4)
delta = abs(mpiHat - (piHat %*% mP))
deltaSum = sum(delta)

stationary_dist = rep(0,4)
for (i in 1:4) {
    e_i = rep(0,4)
    e_i[i] = 1
    e_i = matrix(e_i)
    stationary_dist[i] = abs( mpiHat[i] - ((piHat %*% mP) %*% e_i ) )
}
stationary_dist
sum(stationary_dist)
deltaSum

# g)
#delta = pi - pi*P is close to 0 for n=1000. So piHat is reasonable estimate.

# h)
mP2 = mP %*% mP
mP4 = mP2 %*% mP2
mP8 = mP4 %*% mP4
pi = mP8[1,1:4]
# pi = 0.1969584 0.1336516 0.1357324 0.5336577
# pi close enough when m=8

# i) Find{τ(k), k= 1,...,L}
L<-length(X[X==1])
tau<-order(X)[1:L]

L2=length(X[X==2])
tau2=order(X)[(L+1):(L+L2)]

L3=length(X[X==3])
tau3=order(X)[(L+L2+1):(L+L2+L3)]

L4=length(X[X==4])
tau4=order(X)[(L+L2+L3+1):(L+L2+L3+L4)]

############################
# image 6_1_i_1.jpg
plot(x=1:L4, y=tau4, type="l", col="black")
lines(tau, co="red")
lines(tau2, co="green")
lines(tau3, co="blue")

############################
# image 6_1_i_2.jpg
plot(1:n)
abline(v=tau, col="red")
abline(v=tau2, col="green")
abline(v=tau3, col="blue")
abline(v=tau4, col="black")

############################
#Looking at just event 2 and 3
#They seem to happen with same freq.
#but not always at same time
# either 3 - 4, 3 - 2 or/and 2 - 1
############################
plot(x=1:L2, y=tau2, type="l", col="green")
abline(v=tau2, col="green")
abline(v=tau3, col="black")
##########################

# j)
tau_time = rep(0, L-1)
wait_time = tau[1]
for (i in 2:L) {
    tau_time[i-1] = tau[i] - wait_time
    wait_time = tau[i]
}

#> table(tau_time)
#tau_time
#  1   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  20  21  23  26  33  38 
#126  13   4   9   2   5   5   4   1   4   8   4   2   1   6   2   2   1   1   2   1   2   1 

mean_X0 = sum(tau_time)/(L-1)
#4.742718
#> piHat[1] 
#0.207 
(1/piHat[1] ) - mean_X0
#=0.08819943
# => mean_X0 similar to 1/piHat[1]

# k)
Y<-pmin(pmax(0, X-2),1)
plot(1:n, Y, type='l')

# l)
z = rep(0, n)
z[1] = y[1]
for (i in 2:n) {
    
}




