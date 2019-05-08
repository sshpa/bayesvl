functions{
     int numLevels(int[] m) {
        int sorted[num_elements(m)];
        int count = 1;
        sorted = sort_asc(m);
        for (i in 2:num_elements(sorted)) {
          if (sorted[i] != sorted[i-1])
             count = count + 1;
        }
        return(count);
     }
}
data{
     // Define variables in data
     int<lower=1> Nobs;   // Number of observations (an integer)
     int<lower=0,upper=1> O[Nobs];   // outcome variable
     int<lower=0,upper=1> Lie[Nobs];
     int<lower=0,upper=1> Viol[Nobs];
     int<lower=0,upper=1> VB[Nobs];
     int<lower=0,upper=1> VC[Nobs];
     int<lower=0,upper=1> VT[Nobs];
     int<lower=0,upper=1> Int1[Nobs];
     int<lower=0,upper=1> Int2[Nobs];
}
transformed data{
     // Define transformed data
     vector[Nobs] B_and_Viol;
     vector[Nobs] C_and_Viol;
     vector[Nobs] T_and_Viol;
     vector[Nobs] B_and_Lie;
     vector[Nobs] C_and_Lie;
     vector[Nobs] T_and_Lie;
     int Int1_or_Int2[Nobs];
     int NInt1_or_Int2;
     for (i in 1:Nobs) {
        Int1_or_Int2[i] = (Int1[i]+Int2[i] > 0 ? 1 : 0);
     }
     NInt1_or_Int2 = numLevels(Int1_or_Int2);

     for (i in 1:Nobs) {
        T_and_Lie[i] = VT[i]*Lie[i];
     }

     for (i in 1:Nobs) {
        C_and_Lie[i] = VC[i]*Lie[i];
     }

     for (i in 1:Nobs) {
        B_and_Lie[i] = VB[i]*Lie[i];
     }

     for (i in 1:Nobs) {
        T_and_Viol[i] = VT[i]*Viol[i];
     }

     for (i in 1:Nobs) {
        C_and_Viol[i] = VC[i]*Viol[i];
     }

     for (i in 1:Nobs) {
        B_and_Viol[i] = VB[i]*Viol[i];
     }

}
parameters{
     // Define parameters to estimate
     real b_B_and_Viol_O;
     real b_C_and_Viol_O;
     real b_T_and_Viol_O;
     real b_Viol_O;
     real b_B_and_Lie_O;
     real b_C_and_Lie_O;
     real b_T_and_Lie_O;
     real b_Lie_O;
     real a0_Int1_or_Int2;
     real<lower=0> sigma_Int1_or_Int2;
     vector[NInt1_or_Int2] u_Int1_or_Int2;
}
transformed parameters{
     // Transform parameters
     real theta_O[Nobs];
     vector[NInt1_or_Int2] a_Int1_or_Int2;
     // Varying intercepts definition
     for(k in 1:NInt1_or_Int2) {
        a_Int1_or_Int2[k] = a0_Int1_or_Int2 + u_Int1_or_Int2[k];
     }

     for (i in 1:Nobs) {
        theta_O[i] = b_B_and_Viol_O * B_and_Viol[i] + b_C_and_Viol_O * C_and_Viol[i] + b_T_and_Viol_O * T_and_Viol[i] + b_Viol_O * Viol[i] + b_B_and_Lie_O * B_and_Lie[i] + b_C_and_Lie_O * C_and_Lie[i] + b_T_and_Lie_O * T_and_Lie[i] + b_Lie_O * Lie[i] + a_Int1_or_Int2[Int1_or_Int2[i]+1];
     }
}
model{
     // Priors
     b_B_and_Viol_O ~ normal( 0, 10 );
     b_C_and_Viol_O ~ normal( 0, 10 );
     b_T_and_Viol_O ~ normal( 0, 10 );
     b_Viol_O ~ normal( 0, 10 );
     b_B_and_Lie_O ~ normal( 0, 10 );
     b_C_and_Lie_O ~ normal( 0, 10 );
     b_T_and_Lie_O ~ normal( 0, 10 );
     b_Lie_O ~ normal( 0, 10 );
     a0_Int1_or_Int2 ~  normal(0,5);
     sigma_Int1_or_Int2 ~  normal(0,5);
     u_Int1_or_Int2 ~ normal(0, sigma_Int1_or_Int2);

     // Likelihoods
     O ~ binomial_logit(1, theta_O);
}
generated quantities {
     // simulate data from the posterior
     int<lower=0,upper=1> yrep_O[Nobs];
     // log-likelihood posterior
     vector[Nobs] log_lik_O;
     int<lower=0,upper=1> yrep_Int1_or_Int2_1[Nobs];
     int<lower=0,upper=1> yrep_Int1_or_Int2_2[Nobs];
     for (i in 1:num_elements(yrep_O)) {
       yrep_O[i] = binomial_rng(O[i], inv_logit(theta_O[i]));
     }
     for (i in 1:Nobs) {
       log_lik_O[i] = binomial_logit_lpmf(O[i] | 1, theta_O[i]);
     }
     for (i in 1:Nobs) {
        yrep_Int1_or_Int2_1[i] = binomial_rng(O[i], inv_logit(b_B_and_Viol_O * B_and_Viol[i] + b_C_and_Viol_O * C_and_Viol[i] + b_T_and_Viol_O * T_and_Viol[i] + b_Viol_O * Viol[i] + b_B_and_Lie_O * B_and_Lie[i] + b_C_and_Lie_O * C_and_Lie[i] + b_T_and_Lie_O * T_and_Lie[i] + b_Lie_O * Lie[i] + a_Int1_or_Int2[1]));
     }
     for (i in 1:Nobs) {
        yrep_Int1_or_Int2_2[i] = binomial_rng(O[i], inv_logit(b_B_and_Viol_O * B_and_Viol[i] + b_C_and_Viol_O * C_and_Viol[i] + b_T_and_Viol_O * T_and_Viol[i] + b_Viol_O * Viol[i] + b_B_and_Lie_O * B_and_Lie[i] + b_C_and_Lie_O * C_and_Lie[i] + b_T_and_Lie_O * T_and_Lie[i] + b_Lie_O * Lie[i] + a_Int1_or_Int2[2]));
     }
}
