# 1 d)
myx = seq(1,200,1)
geomy = dgeom(myx-1, prob=1/36)
plot(myx, geomy, type="h", col="red", lwd=1.5, xlab = "", ylab = "p(x)")

myx = seq(1,200,1)
expoy = dexp(myx-1, rate=1/36)
lines(expoy, co="blue")
#plot(myx, geomy, type="h", col="red", lwd=1.5, xlab = "", ylab = "p(x)")

# 2 b)
lambdaA = 1/100
lambdaB = 1/900
lambdaU = lambdaA + lambdaB
avgA = 1/lambdaA
avgB = 1/lambdaB
avgU = 1/lambdaU
range = seq(from = 0, to = 400, by = 10)
XA = dexp(range, rate=lambdaA)
XB = dexp(range, rate=lambdaB)
U = dexp(range, rate=lambdaU)
plot(range, XA, type = "l", col = "green", main = "exponential distributions",
     ylab = "probability", xlab = "time")
lines(range, XB, col = "red")
lines(range, U, col = "blue")

# 2 c)
n=10^3
m=10^2
N=n*m
#i
Ax = rexp(N, lambdaA)
Bx = rexp(N, lambdaB)
#ii
ABx = cbind(Ax,Bx)
U = pmin(Ax, Bx)
#Z = 1*(Bx>Ax)
Z = ifelse(Ax < Bx, 0, 1)
U_output = sort(U, index.return=TRUE)
U_sort = U_output$x
U_idx = U_output$ix
Z_sorted = Z[order(U_idx)]

#U_sort <- U[order(U)]
#Z_sorted <- Z[order(U)]

V = c(rep(0,m))
Y = c(rep(0,m))
for (j in 1:m){
    for (i in 1:n) {
        V[j] = V[j] + U_sort[m*(j-1)+i]
        Y[j] = Y[j] + Z_sorted[m*(j-1)+i]        
    }
}
V = (1/m)*V
Y = (1/m)*Y
plot(Y,V)

#***************************
# The two variables are independent because the plot is caotic. (Looks like 'snow')
#***************************

#4.5
N=10^6
blodtype = c(0.34,0.408, 0.068, 0.034, 0.06, 0.072, 0.012, 0.0006)
accum_blodtype = 0
n = c(0,0,0,0,0,0,0,0)
p = c(0,0,0,0,0,0,0,0)
X = c(0,0,0,0,0,0,0,0)
size_accum = 0
for (i in 1:8) {
    p[i] = blodtype[i]/(1-accum_blodtype)
    accum_blodtype = accum_blodtype + blodtype[i]
    #print(accum_blodtype)
    n[i] = N - size_accum
    X[i] = rbinom(1, n[i], p[i]) 
    size_accum = size_accum + X[i]
    print(size_accum)
}
p
rel_X = X/N
#0.3420 0.4024 0.0706 0.0353 0.0593 0.0739 0.0117 0.0001 ----- k=1
#0.339658 0.408662 0.067796 0.034064 0.060183 0.071451 0.012121 0.000638 ---- k=3

#*****************
# a) rel_X is similar to blodtype 
# b) precision of 10^2k... is incleasing when k is increasing
#******************







