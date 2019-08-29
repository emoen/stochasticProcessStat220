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