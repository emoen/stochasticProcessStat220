n=10
i=replicate(n, 0)

w_0 = 0
i_j_minus1 = w_0
num = 0.1245

wk=replicate(n, 0)
i[0] = floor(2*num)
i_previous = 2*num
for (j in 2:4) {
    i_j_minus1 = 2^j*(num-i_previous)
    print(i_previous)
    i[j] = floor(i_previous)
    i_previous = 2^j*(num - i_previous)
    wk[j] = wk[j-1]+2^j*i[j]
}