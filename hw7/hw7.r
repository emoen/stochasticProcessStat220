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

# f) 
n1=10000
Y <- simulate.repairman(n=n1,lambda=lambda, mu=mu, x0 = 6, R=1)
plot(X, type="s")

time_steps = Y[,1]
dt=diff(time_steps) # dt[x] = time_steps[x+1] - time_steps[x]. E.g step 8 entered state 1. df[8]=0.6481629 time spendt in state 1 (until enter state 2 or 0)
cumtime_in_states = rep(0,6) 
for(i in 1:6){
    array_step_x_entered_state_i = which(Y[-n1,2]==i) 
    cumtime_in_states[i] = sum(dt[array_step_x_entered_state_i])
}
l= cumtime_in_states
#dpois - PMF gives the (log) density - poisson
#ppois - CMF Cumulative Probability Function
pi_theoretic = dpois(0:5,lambda=lambda/mu)/ppois(5,lambda=lambda/mu) # pi := PMF/CMF == truncated poisson at N=5
rbind(l/sum(l), pi_theoretic)

#12.2 d)
plot( dpois( x=0:5, lambda=2 ))
dat = data.frame(x=c(0,1,2,3,4,5), y=pi_theoretic)
hist(pi_theoretic)

# g) / h)
# 1) empirical avg machines operated
# 2)               machine utilization
# 3)               idle repair time - no machines working

total_time = sum(l)
emp_machines_op = sum(l*0:5/total_time) # E[M] = sum( i * total_time[i] ), i:0..5
emp_util = sum(l*c(0:5)/5/sum(l)) # Utilization 
emp_state_0 = l[6]/total_time

#Theoretic time
machines_op = sum(pi_theoretic * (0:5) ) # E[M] = sum( i * total_time[i] ), i:0..5
util = sum(pi_theoretic*(0:5)/5)         # pi * state_i / num_states_of_utility
state_0 = pi_theoretic[6] # state of zero utility
comparison = matrix(c(emp_machines_op, emp_util, emp_state_0, machines_op, util, state_0),nrow=3,ncol=2)

# j)
n1=10000
Y <- simulate.repairman(n=n1,lambda=lambda, mu=mu, x0 = 6, R=2)
plot(X, type="s")

################## get new statistics ########################
time_steps = Y[,1]
dt=diff(time_steps) # dt[x] = time_steps[x+1] - time_steps[x]. E.g step 8 entered state 1. df[8]=0.6481629 time spendt in state 1 (until enter state 2 or 0)
cumtime_in_states = rep(0,6) 
for(i in 1:6){
    array_step_x_entered_state_i = which(Y[-n1,2]==i) 
    cumtime_in_states[i] = sum(dt[array_step_x_entered_state_i])
}
l= cumtime_in_states

total_time2 = sum(l)
emp_machines_op2 = sum(l*0:5/total_time) # E[M] = sum( i * total_time[i] ), i:0..5
emp_util2 = sum(l*c(0:5)/5/sum(l)) # Utilization 
emp_state_02 = l[6]/total_time

#Theoretic time
machines_op2 = sum(pi_theoretic * (0:5) ) # E[M] = sum( i * total_time[i] ), i:0..5
util2 = sum(pi_theoretic*(0:5)/5)         # pi * state_i / num_states_of_utility
state_02 = pi_theoretic[6] # state of zero utility
comparison2 = matrix(c(emp_machines_op2, emp_util2, emp_state_02, machines_op2, util2, state_02),nrow=3,ncol=2)
##################################################################

> comparison2
           [,1]       [,2]
[1,] 1.94542510 1.92660550
[2,] 0.60533441 0.38532110
[3,] 0.06840811 0.03669725
> comparison
           [,1]       [,2]
[1,] 2.06377724 1.92660550
[2,] 0.41275545 0.38532110
[3,] 0.02662338 0.03669725
> 

# k)
# Doesnt change theoretical PI as num repairmen isnt used in calculation

# A second repairman encrease utilization and also zero-utility/idle repairtime




