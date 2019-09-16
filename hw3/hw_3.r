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
for (j in 1:n){
    for (i in 1:m) {
        V[j] = V[j] + U_sort[m*(j-1)+i] 
    }
    V[j] = (1/m)*V[j]
}







