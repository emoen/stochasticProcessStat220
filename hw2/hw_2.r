#oppgave 2.1
n=12
i=replicate(n, 0)
w=replicate(n, 0)

w_0 = 0
i_j_minus1 = w_0
num = 0.1245

i[1] = floor(2*num)
i_previous = 2*num
w[1]= i[1]*1/2
for (j in 2:n) {
    i[j] = floor(2*i_previous)
    print(2*i_previous)
    i_previous = 2*i_previous - floor(2*i_previous)  
    w[j] = w[j-1] + i[j]*1/(2^j)
}

print(i)
#[1] 0 0 0 1 1 1 1 1 1 1 0 1
print(w)
# [1] 0.0000000 0.0000000 0.0000000 0.0625000 0.0937500 0.1093750 0.1171875
# [8] 0.1210938 0.1230469 0.1240234 0.1240234 0.1242676
1/16
#[1] 0.0625
3/32
#[1] 0.09375

##################
#oppgave 2.2 d
n = 15
I = rbinom(n, 1, 0.5)
j =1:n
U = 0

for (j in 1:n){
    U = U +(2^-j)*I[j]
    next
}

# assert ( I = c(1,1,1,1) ) => K=15 ) ok
K =0
for (j in 1:n){
    print(j)
    if (I[n-j+1]==1)
        K = K+2^(j-1)
    next
}
K

# Upper bound of matrix (Ank[[j,2]]) is exclusive
Ank = matrix(list(), nrow=2^n, ncol=2)
for (j in 1:2^n){
   print(j/n)
   Ank[[j,1]] = (j-1)/2^n
   Ank[[j,2]] = j/2^n
   next 
}


x=runif(n=1, min=0, max=.9999999999)
# 2.3 d)
# x = 0.10001
# x = 0.09999
Fn_x=0
for (j in 0:((2^n)-1)) {
    if (x >= Ank[[j+1,1]] && x < Ank[[j+1,2]]) {
        print(j)
        Fn_x = Fn_x + (j+1)/(2^n)
    }
}
Fn_x