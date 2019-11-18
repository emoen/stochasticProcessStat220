#12.2 d)
plot( dpois( x=0:5, lambda=2 ))

#Function for simulating repairman problem
simulate.repairman <- function(n=100,lambda=2, mu=1, x0 = 6, R = 1){
    # n - numeric of transitions# lambda - birth rate
    # mu - death rate
    # x0 - initial state (1,2,3,4,5,6)
    # R - number of repairmen
    lambdavec = rep(0,6)  # = [2,2,2,2,1,0] (lambda is 2 but only one machine can be fixed last transition)
    muvec = rep(0,6)      # = [0,1,2,3,4,5]  
    lambdavec[6] = 1/1e32 # ~ 0 - last lambda is 0
    muvec[1] = 1/1e32 # ~ 0 - first mu is 0 in (finite) random walk
    for(i in (1:5)) {
        lambdavec[i] = lambda * min(c(6-i,R))
        print((c((6-i), R)))
    }
    muvec[2:6] = mu * 1:5
    x = t = rep(0,n)
    x[1] = x0 #initializing
    dx = c(1,-1) # 1 => one repair, step up. -1 => one break_down, step down
    for(i in 2:n){
        # Drawing traisition
        # rexp - sampling from the exponential
        repair_machine = rexp(1, rate = lambdavec[x[i-1]]) #no machine to repair when x=6 (lambadec[6]=0.0..01) rexp(1,rate=0.0..01)=infinity
        break_down_machine = rexp(1, rate = muvec[x[i-1]]) #x=6, mu=5, e(breakdown) = 1/5
        E = c(repair_machine, break_down_machine)
        step_up_or_down = which(E==min(E)) #i=2, min (E) most certainly break_down_machine => which(E=min(E)) = 2
        x[i] = x[i-1]+ dx[step_up_or_down]
        t[i] <- min(E) # continous time on x-axis. min value drawn from exponential dist is waiting-time until next event.
    }
    return(cbind(cumsum(t),x)) # Returning simulation, x-axis cumulative sum of sampling from exponential dist. Y-axis is the event of going up or down -> which(min(E))
}

set.seed(1)

# e)
lambda = 2
mu = 1
X <- simulate.repairman(n=100,lambda=lambda, mu=mu, x0 = 6, R=1)
plot(X, type="s")
