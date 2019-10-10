init3<-rep(0.33,3)
mP3<-matrix(0,3,3)
S3 <-c(1:3)  # state space
mP3[1,]<- c(0,1,0)
mP3[2,]<- c(0.6,0,0.4)
mP3[3,]<-c(0,1,0)

init4<-rep(0.25,4)
mP4<-matrix(0,4,4)
S4 <-c(1:4)  # state space
mP4[1,]<- c(0,1,0,0)
mP4[2,]<- c(0.6,0,0.4,0)
mP4[3,]<-c(0,0.6,0,0.4)
mP4[4,]<-c(0,0,1,0)

########
# 8.1 a)

birth_death_N <- function(p, N) {
    init = rep(1/N, N)
    mpN = matrix(0, N,N)
    SN = c(1:N)
    for (i in 1:N) {
        if ( i == 1) {
            mpN[1,2] = 1
        } else if ( i == N ) {
            mpN[N,N-1] = 1
        } else {
            mpN[i,i-1] = 1-p
            mpN[i,i+1] = p        
        }
    }
    return(list("init"=init, "mpN"=mpN, "SN"=SN))
}

iter = 1000
simulate_plot = function( init, mP, S, iter) {
    n=iter
    X = c(rep(0,n+1))
    X[1]<-sample(S,1, prob=init )
    k=1
    for (k in 2:(n+1)) {
        X[k]<-sample(S,1, prob=mP[X[k-1],] )
    }
    X = X[-1]
    plot(1:n, X, type="s", col="blue")
}

PP = 0.4
N=3
record = birth_death_N(PP, N)
init = record$init
mP = record$mpN
S = record$SN
simulate_plot(init, mP, S, iter)

N=4
record = birth_death_N(PP, N)
init = record$init
mP = record$mpN
S = record$SN
simulate_plot(init, mP, S, iter)

#########
# b)
N=10
record = birth_death_N(PP, N)
init = record$init
mP = record$mpN
S = record$SN
simulate_plot(init, mP, S, iter)

#########
# c)
qq = 1-PP
cumProd = (1/qq)*(PP/qq)^(N-3)*(PP)
piK = 


