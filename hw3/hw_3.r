# 1 d)
myx = seq(1,200,1)
geomy = dgeom(myx-1, prob=1/36)
plot(myx, geomy, type="h", col="red", lwd=1.5, xlab = "", ylab = "p(x)")

myx = seq(1,200,1)
geomy = dexp(myx-1, rate=1/36)
lines(geomy, co="blue")
#plot(myx, geomy, type="h", col="red", lwd=1.5, xlab = "", ylab = "p(x)")


