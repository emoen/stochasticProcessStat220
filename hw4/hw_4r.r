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
hist(tau_time)

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

# l) rain days in bergen
z = rep(0, n)
z[1] = Y[1]
for (i in 2:n) {
    z[i] = (z[i-1]+1)*Y[i]
}
plot(1:n, z)

# m) find piHat with n=50*365 (50 years)
###################
# copy - past from top
n=50*365
X2 = c(rep(0,n+1))
X2[1]<-sample(S,1, prob=init )
k=1
for (k in 2:(n+1)) {
    X2[k]<-sample(S,1, prob=mP[X2[k-1],] )
}
X2 = X2[-1]
plot(1:n, X2, type="s", col="blue")

piHat2 = table(X2)/n

#####################
# piHat with 50 years is similar to 1000 days:
#> piHat2
#X2
#        1         2         3         4 
#0.2041096 0.1356164 0.1356164 0.5246575 
#> piHat
#X
#    1     2     3     4 
#0.207 0.138 0.137 0.518 

# n) 
#get pi from eigen vector with eigen value = 1
library(MASS)
e = eigen(mP)
left_vector = ginv(e$vectors)
pi_eig = left_vector[1,]/sum(left_vector[1,])
sum(pi_eig)
#> pi_eig
#[1] 0.1943463 0.1342756 0.1342756 0.5371025
#> piHat2
#X2
#        1         2         3         4 
#0.2041096 0.1356164 0.1356164 0.5246575 
##############
#pi_eig is close to piHat2
#############

# o) rainiest days in 50 years according to model
Y2<-pmin(pmax(0, X2-2),1)
z2 = rep(0, n)
z2[1] = Y2[1]
for (i in 2:n) {
    z2[i] = (z2[i-1]+1)*Y2[i]
}
max(z2)
# z2 = 36 vs z=24", # 103 regndager registert i 1986

# p)
L50<-length(X2[X2==1])
tau50<-order(X2)[1:L50]
tau_time2 = rep(0, L50-1)
wait_time2 = tau50[1]
for (i in 2:L50) {
    tau_time2[i-1] = tau50[i] - wait_time2
    wait_time2 = tau50[i]
} 
d = density(tau_time2)
hist(tau_time2)
plot(d, col="red")

#> mean(tau_time2)
#[1] 4.942818
mean_expo = mean(tau_time2)

#mean = 1/rate
rate_expo = 1/(mean_expo)
hist(rexp(L50-1, rate_expo))
expo_x = (1:(L50-1))
expo_y = dexp((1:length(tau_time2)), rate=(rate_expo))
geom_y = dgeom(1:length(tau_time2), (rate_expo))
#plot(x=expo_x, y=expo_y, type='l')
lines(expo_y)
lines(geom_y)

t2 = tau_time2[order(tau_time2)]
#remove all 1's
t3 = t2[2285:(L50-1)]
plot(density(t3))
plot(density(t3[1081:length(t3)]))

plot(1:(50*365), X2[order(X2)], type="l")

#*********** q) see notes ********************
#6.2
nP<-matrix(0,3,3)
nP[1,]<- c(0.7,0.2,0.1)
nP[2,]<- c(0.0,0.60,0.4)
nP[3,]<-c(0.5,0.0,0.5)

np2 = nP %*% nP
np3 = np2 %*% np2
np4 = np3 %*% nP

#************************
#6.5 a)
nu = t(mP[1,])         # the first row vector of mP
mI = diag(rep(1,4))    # unit matrix
s = mI[,1]             # first unit vector, e_1, in R**4
msnu = s %*% nu
mR = mP - msnu

mP # P
mR # R
s  # = e_0
nu # =v
s %*% nu

#************************
#6.5 b)


