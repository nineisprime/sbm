
K = 3
n = 1000
m = 1000

ntrials = 10000

P = runif(K); P = P/sum(P);
#Q = runif(K); Q = Q/sum(Q);
Q = P + runif(K)*0.2; Q = Q/sum(Q)

renyi = -2*log(sum(sqrt(P*Q)))
upper_bound = exp(-renyi*n)
lower_bound = 0.25*exp(-2*renyi*n)

#lower_bound = 0.5*(sum(sqrt(P*Q)))^4

count = 0
for (it in 1:ntrials){
    
    Xn = sample(1:K, n, replace=TRUE, prob=P)
    Ym = sample(1:K, m, replace=TRUE, prob=Q)

    Xtilde = log(P[Xn]/Q[Xn])
    Ytilde = log(P[Ym]/Q[Ym])

    if (sum(Xtilde) < sum(Ytilde)){
        
        count = count+1

        #if (sum(Xn-1) > sum(Ym-1) && P[2] > Q[2] )
        #    print("ERR!")
        #if (sum(Xn-1) < sum(Ym-1) && P[2] < Q[2] )
        #    print("ERR!")
    }

    
}

probab = count/ntrials
print(c(lower_bound, probab, upper_bound))
