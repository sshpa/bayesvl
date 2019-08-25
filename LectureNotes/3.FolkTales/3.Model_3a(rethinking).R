data1<-read.csv("/Statistics/Lie/20180224_DataTable_345.csv", header = TRUE)
head(data1)

# Load libraries ----
library(rstanarm)
library(brms)  # for models
library(bayesplot)
library(ggplot2)
library(dplyr)
library(tidybayes)
library(modelr)

options(mc.cores = parallel::detectCores())


stan_glm1 <- stan_glm(O ~ VB*Viol + VC*Viol + VT*Viol + Viol + VB*Lie + VC*Lie + VT*Lie + Lie,
                     data = data1, family = binomial,
                     chains = 4, cores = 4)
                     
library(rethinking)

mB.1 <- map2stan(
alist(
O ~ dbinom( 1 , lp ) ,
logit(lp) <- b_B_and_Viol_O * VB*Viol + b_C_and_Viol_O * VC*Viol + b_T_and_Viol_O * VT*Viol + 
	b_Viol_O * Viol + b_B_and_Lie_O * VB*Lie + b_C_and_Lie_O * VC*Lie + b_T_and_Lie_O * VT*Lie + b_Lie_O * Lie + a_Int1_or_Int2[Int1 + Int2],
	b_B_and_Viol_O ~ normal( 0, 10 ),
	b_C_and_Viol_O ~ normal( 0, 10 ),
	b_T_and_Viol_O ~ normal( 0, 10 ),
	b_Viol_O ~ normal( 0, 10 ),
	b_B_and_Lie_O ~ normal( 0, 10 ),
	b_C_and_Lie_O ~ normal( 0, 10 ),
	b_T_and_Lie_O ~ normal( 0, 10 ),
	b_Lie_O ~ normal( 0, 10 ),
	a0_Int1_or_Int2 ~  normal(0,5),
	sigma_Int1_or_Int2 ~  normal(0,5),
	u_Int1_or_Int2 ~ normal(0, sigma_Int1_or_Int2)
) , 
chains = 4,
data=Legends345)

