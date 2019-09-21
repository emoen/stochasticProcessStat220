# 1 d)
myx = seq(1,200,1)
geomy = dgeom(myx-1, prob=1/36)
plot(myx, geomy, type="h", col="red", lwd=1.5, xlab = "", ylab = "p(x)")

myx = seq(1,200,1)
expoy = dexp(myx-1, rate=1/36)
lines(expoy, co="blue")
#plot(myx, geomy, type="h", col="red", lwd=1.5, xlab = "", ylab = "p(x)")

# 2 b)
myx = seq(1,200,1)
Ay = dexp(myx-1, rate=1/100)
By = dexp(myx-1, rate=1/900)
Zy = dexp(myx-1, rate=1/1000)
Zy = dexp(myx-1, rate=9/10)
plot(myx, By, type="l", col="red")
lines(Zy, co="blue")
lines(Ay, co="green")

# 2 c)
n=10^3
m=10^3
N=n*m
#i
Ax = rexp(N, rate=(1/100))
Bx = rexp(N, rate=(1/900))
#ii
ABx = cbind(Ax,Bx)
U = pmin(Ax, Bx)
Z = 1*(Bx>Ax)
U_output = sort(U, index.return=TRUE)
U_sort = U_output$x
U_idx = U_output$ix
Z_sorted = Z[order(U_idx)]
V = c(1:m)
Y = c(1:m)
for (j in 1:m){
    for (i in 1:n) {
        V[j] = V[j] + U_sort[m*(j-1)+i]
        Y[j] = Y[j] + Z_sorted[m*(j-1)+i]        
    }
    V[j] = (1/m)*V[j]
    Y[j] = (1/m)*Y[j]
}
plot(Y,V)

#plot(c(1:m), V, type="l", col="red")
#lines(Y, col="blue")

#4.5
N=10^4
blodtype = c(0.34,0.408, 0.068, 0.034, 0.06, 0.072, 0.012, 0.0006)
accum_blodtype = 0
n = c(0,0,0,0,0,0,0,0)
p = c(0,0,0,0,0,0,0,0)
X = c(0,0,0,0,0,0,0,0)
size_accum = 0
for (i in 1:8) {
    p[i] = blodtype[i]/(1-accum_blodtype)
    accum_blodtype = accum_blodtype + blodtype[i]
    print(accum_blodtype)
    n[i] = N - size_accum
    size_accum = size_accum + n[i]
    X[i] = rbinom(1, n[i], p[i])    
}
p







