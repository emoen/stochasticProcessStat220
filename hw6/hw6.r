#Method 1:
#This algorithm exploits the fact that interarrival times are exponentially distributed. 
#We simulate the arrival times until the maximum time horizon is achieved.

# 2 methods for simulating poisson process
# https://www.r-bloggers.com/%F0%9F%93%88-simulating-poisson-process-part-1/

#link inter- arrival, arrival, and number of arrivals at time
# https://towardsdatascience.com/the-poisson-process-everything-you-need-to-know-322aa0ab9e9a

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

###########################
#Method 2
#This method simulates the number of jumps by Possion random variable 
#with the rate equals to the product of the time horizon and the process’s rate. 
#Then, to calculate arrival times, random variables with uniform distribution 
#are generated and ordered after

poissonProcess = c(rep(0,t))
rates = c(rep(0,t))
for ( i in c(1:t)) {
    rate = lambdaExp*i
    poissonProcess[i] = rpois(1, rate)
    rates[i]=rate
}
poissonProcess
rate

jumpsNumber = c(rep(0,t))
jumpsNumber[1]=poissonProcess[1]
for ( i in c(2:t)){
    jumpsNumber[i] = jumpsNumber[i-1]+poissonProcess[i]
}
jumpsNumber
plot(c(1:t), poissonProcess)