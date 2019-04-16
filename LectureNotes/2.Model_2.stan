data{
    int<lower=1> N;
    int<lower=0,upper=1> B[N];
    int<lower=0,upper=1> E[N];
    int<lower=0,upper=1> C[N];
}
parameters{
    real<lower=0,upper=1> pB;
    real<lower=0,upper=1> pE;
    real<lower=0,upper=1> pC;
}
model{
    pB ~ beta(1, 1);
    pE ~ beta(1, 1);
    pC ~ beta(1, 1);
    
    for ( i in 1:N ) {
        B[i] ~ bernoulli(pB);
        E[i] ~ bernoulli(pE);
        C[i] ~ bernoulli(pC);
    }
}
