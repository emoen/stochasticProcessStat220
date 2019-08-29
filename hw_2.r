n=12
i=replicate(n, 0)

w_0 = 0
i_j_minus1 = w_0
num = 0.1245

wk=replicate(n, 0)
i[1] = floor(2*num)
i_previous = 2*num
for (j in 2:n) {
    i[j] = floor(2*i_previous)
    print(2*i_previous)
    i_previous = 2*i_previous - floor(2*i_previous)  
}

#    wk[j] = wk[j-1]+2^j*i[j]