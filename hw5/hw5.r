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
# c Implement (4) and find pi_0 .. pi_n
qq = 1-PP

Phi = rep(0, N)
Phi[1] = 1
Phi[2] = 1/qq
if ( N>3) {
    for (i in 3:(N-1)) {
        Phi[i] = Phi[i-1]* (PP/qq)
    }
}
Phi[N] = Phi[N-1]*PP

acumSum = sum(Phi)
Pi_k = rep(0, N)
for ( i in 1:N) {
    Pi_k[i] = Phi[i] / acumSum
}

######################
# 8.3 c -d
######################
aaa =  matrix(0,6,6)
aaa[1,] = c(1/6,1/6,1/6,1/6,1/6,1/6)
aaa[2,] = c(1/4, 1/4, 1/4, 0, 1/4, 0)
aaa[3,] = c(0,0,1/3, 2/3, 0,0)
aaa[4,] = c(0,0,2/3, 1/3, 0,0)
aaa[5,] = c(0,0,0,0,1/4, 3/4)
aaa[6,] = c(0,0,0,0,1/5,4/5)


aaa_2 = aaa %*% aaa
for (i in 1:10) {  # 2^11 = 2048
    aaa_2 = aaa_2 %*% aaa_2
}
print(aaa_2)

init= aaa[1,]
n=100
S=c(1:6)
X = c(rep(0,n+1))
X[1]<-sample(S,1, prob=init )
X[1] = 2
for(k in 2:(n+1)) {
    print(X)
    X[k] = sample(S,1, prob=aaa[X[k-1],] )
}
X = X[-1]
empirical_freq = table(X)/n

###########
U = matrix(0,2,4)
U[1,] = c(0.43,0.07,0.43,0.07)
U[2,] = c(0.29,0.21,0.29,0.21)

y_1=c(0.5,0.5)
y_2=c(0.5,0.5)
y_1 = t(t(y_1))
y_2 = t(t(y_2))

#### incorrect #####
aaa_C = matrix(0,2,2)
aaa_C[1,]=c(1/3,2/3)
aaa_C[2,]=c(1,1)#c(2/3,1/3)

id=matrix(0,2,2)
id[1,1]=1
id[2,2]=1

solve((id-aaa_C), c(0,1))

aaa_D = matrix(0,2,2)
aaa_D[1,]=c(1/4,3/4)
aaa_D[2,]=c(1/5,4/5)
##### end incorrect #######

####
# d)
####
z_0 = c(0.5, 0.5)
j=3
first_term = aaa[1:2, j]

z_1 = first_term + aaa[1:2,1:2] %*% z_0
for (i in 1:10) {
    z_1 = first_term + aaa[1:2,1:2] %*% z_1
}

#################
# 8.2
################
mmp = matrix(0,3,3)
mmp[1,] = c(0.5,0.5,0)
mmp[2,] = c(0.5,0,0.5)
mmp[3,] = c(0,0.25, 0.75)
init= c(0.33,0.33,0.33)
n=10000
S=c(1:3)
X = c(rep(0,n+1))
X[1]<-sample(S,1, prob=init )
for(k in 2:(n+1)) {
    X[k] = sample(S,1, prob=mmp[X[k-1],] )
}
X = X[-1]
empirical_freq = table(X)/n




