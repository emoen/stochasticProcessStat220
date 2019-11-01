#Method 1:
#This algorithm exploits the fact that interarrival times are exponentially distributed. 
#We simulate the arrival times until the maximum time horizon is achieved.

lambdaExp = 2
t=10
T = c(rep(0, t)) # inter-arrival time - exponential distributed
for (i in c(1:t)){
    T[i] = rexp(1, rate = lambdaExp) #sample for exponential dist
}
plot(c(1:t), T)

S = c(rep(0, t)) # arrival times - gamma distributed
S[1] = T[1]
for (i in c(2:t)){
    S[i]= S[i-1] + T[i]
}
plot(c(1:t), S)

X = c(rep(0,t)) # number of arrivals up to time t - poisson process distributed (lambda*t)
for (i in c(1:t)){
    for (j in c(1:t)) {
        if ( S[i] < j ) {
            X[j] = X[j] + 1
        }
    }
}
plot(c(1:t), X)

########
poissonProcess = c(rep(0,t))
for ( i in c(1:t)) {
    rate = lambdaExp*i
    poissonProcess[i] = rpois(1, rate)
}
plot(c(1:t), poissonProcess)