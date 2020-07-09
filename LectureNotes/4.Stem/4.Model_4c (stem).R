dat <- read.csv("/Users/Shared/Previously Relocated Items/Security/Statistics/STEM/New/5000Toan.csv", header = T)
head(dat)

require(dplyr)
dat %>% count(TimeSci, TimeSoc)

filtered <- dat[!is.na(dat$TimeSoc),]
filtered <- droplevels(filtered)

filtered %>% count(TimeSci, TimeSoc)

filtered <- dat[!is.na(dat$Buybook) & !is.na(dat$Readstory) & (dat$Readbook %in% c("yes", "no")),]
filtered <- droplevels(filtered)

filtered$Readstory <- filtered$Readstory - 1
filtered$Buybook <- filtered$Buybook - 1
filtered$Readbook <- as.numeric(filtered$Readbook) - 1

filtered %>% count(Readbook)

filtered$Typebook <- ifelse(dat$Typebook %in% c(1,2,3,4,5), dat$Typebook, 6)

sr<-as.data.frame(filtered %>% count(Source))

############################### By APS45 #################

filtered <- dat[!is.na(dat$Hobby),]
filtered <- droplevels(filtered)

model_string <- 
"data{
    int<lower=1> Nobs;
    int<lower=1> NHobby;
    real APS45[Nobs];
    int<lower=1,upper=NHobby> Hobby[Nobs];
}
parameters{
    real theta[NHobby];
    real sigma;
}
model{
		for(k in 1:NHobby) {
		  theta[k] ~ normal(0, 10);
		}
    
    APS45 ~ normal(theta[Hobby], sigma);
}
"

datHobby <- with(filtered,
            list(Nobs         = length(APS45),
                 APS45          = as.numeric(APS45),
                 Hobby          = as.numeric(Hobby),
                 NHobby = length(unique(Hobby))
                 ))

mstanHobby <- rstan::stan(model_code = model_string, data = datHobby,
        		warmup=2000 , iter = 4000, chains = 4)

exclude <- c("lp__", "sigma")
par_names <- setdiff(dimnames(mstanHobby)[[3]], exclude)

postParams <- rstan::extract(mstanHobby, pars = par_names)
names(postParams) <- unique(filtered$Hobby)
ref <- reshape2::melt(postParams)
colnames(ref)[2:3] <- c("value","Hobby")

library(ggplot2)

ggplot(data=ref,aes_string(x="value", color="Hobby"))+geom_density(size=1)+
xlab("") + ylab("") +
theme_bw(base_size=15) +
theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), legend.title = element_blank(), legend.position = c(0.85, 0.85)) 


############################### By Readbook #################

filtered <- dat[(dat$Readbook %in% c("yes", "no")),]
filtered <- droplevels(filtered)

filtered$Readbook <- ordered(filtered$Readbook, levels = c("no", "yes"))

ap <- as.data.frame(filtered %>% count(Readbook, APS45ID))

model_string <- 
"data{
    int<lower=1> Nobs;
    int<lower=1> NReadbook;
    real APS45[Nobs];
    int<lower=1,upper=NReadbook> Readbook[Nobs];
}
parameters{
    real theta[NReadbook];
    real sigma;
}
model{
		for(k in 1:NReadbook) {
		  theta[k] ~ normal(0, 10);
		}
    
    APS45 ~ normal(theta[Readbook], sigma);
}
"

datASP45 <- with(filtered,
            list(Nobs         = length(APS45),
                 APS45          = as.numeric(APS45),
                 Readbook          = as.numeric(Readbook),
                 NReadbook = length(unique(Readbook))
                 ))

mstanASP45 <- rstan::stan(model_code = model_string, data = datASP45,
        		warmup=2000 , iter = 4000, chains = 4)

exclude <- c("lp__", "sigma")
par_names <- setdiff(dimnames(mstanASP45)[[3]], exclude)

postParams <- rstan::extract(mstanASP45, pars = par_names)
names(postParams) <- levels(filtered$Readbook)
ref <- reshape2::melt(postParams)
colnames(ref)[2:3] <- c("value","Readbook")

library(ggplot2)

ggplot(data=ref,aes_string(x="value", color="Readbook"))+geom_density(size=1)+
xlab("") + ylab("") +
theme_bw(base_size=15) +
theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), legend.title = element_blank(), legend.position = "bottom") 


############################### By MostlikedAct #################
filtered <- filtered[!is.na(filtered$MostlikedAct),]
filtered <- droplevels(filtered)

model_string <- 
"data{
    int<lower=1> Nobs;
    int<lower=1> NMostlikedAct;
    int<lower=0,upper=1> Readbook[Nobs];
    int<lower=1,upper=NMostlikedAct> MostlikedAct[Nobs];
}
parameters{
    real<lower=0,upper=1> theta[NMostlikedAct];
}
model{
		for(k in 1:NMostlikedAct) {
		  theta[k] ~ beta(1, 1);
		}
    
    Readbook ~ bernoulli(theta[MostlikedAct]);
}
"

datReadbook <- with(filtered,
            list(Nobs         = length(Readbook),
                 Readbook          = as.numeric(Readbook),
                 NMostlikedAct = length(unique(MostlikedAct)),
                 MostlikedAct     	      = as.numeric(MostlikedAct)))

mstanMostlikedAct <- rstan::stan(model_code = model_string, data = datReadbook,
        		warmup=2000 , iter = 4000, chains = 2)

exclude <- c("lp__")
par_names <- setdiff(dimnames(mstanMostlikedAct)[[3]], exclude)

postParams <- rstan::extract(mstanMostlikedAct, pars = par_names)
names(postParams) <- unique(filtered$MostlikedAct)
ref <- reshape2::melt(postParams)
colnames(ref)[2:3] <- c("value","MostlikedAct")

library(ggplot2)

#listLabel <- c("Buybook", "Readstory")

ggplot(data=ref,aes_string(x="value", color="MostlikedAct"))+geom_density(size=1)+
xlab("") + ylab("") +
#scale_color_discrete(labels = listLabel) +
theme_bw(base_size=15) +
theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), legend.title = element_blank(), legend.position = c(0.85, 0.85)) 

############################### By Sex #################
filtered <- filtered[!is.na(filtered$Sex),]
filtered <- droplevels(filtered)

model_string <- 
"data{
    int<lower=1> Nobs;
    int<lower=1> NSex;
    int<lower=0,upper=1> Readbook[Nobs];
    int<lower=1,upper=NSex> Sex[Nobs];
}
parameters{
    real<lower=0,upper=1> theta[NSex];
}
model{
		for(k in 1:NSex) {
		  theta[k] ~ beta(1, 1);
		}
    
    Readbook ~ bernoulli(theta[Sex]);
}
"

datReadbook <- with(filtered,
            list(Nobs         = length(Readbook),
                 Readbook          = as.numeric(Readbook),
                 NSex = length(unique(Sex)),
                 Sex     	      = as.numeric(Sex)))

mstanSex <- rstan::stan(model_code = model_string, data = datReadbook,
        		warmup=2000 , iter = 4000, chains = 2)

exclude <- c("lp__")
par_names <- setdiff(dimnames(mstanSex)[[3]], exclude)

postParams <- rstan::extract(mstanSex, pars = par_names)
names(postParams) <- unique(filtered$Sex)
ref <- reshape2::melt(postParams)
colnames(ref)[2:3] <- c("value","Sex")

library(ggplot2)

listLabel <- c("Male", "Female")

ggplot(data=ref,aes_string(x="value", color="Sex"))+geom_density(size=1)+
xlab("") + ylab("") +
scale_color_discrete(labels = listLabel) +
theme_bw(base_size=15) +
theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), legend.title = element_blank(), legend.position = "bottom") 

#######################

model_string <- 
"data{
     // Define variables in data
     int<lower=1> Nobs;   // Number of observations (an integer)
     int<lower=0,upper=1> Readbook[Nobs];   // outcome variable
     int<lower=0,upper=1> Buybook[Nobs];
     int<lower=0,upper=1> Readstory[Nobs];
     int NSex;
     int<lower=1,upper=NSex> Sex[Nobs];
}
parameters{
     // Define parameters to estimate
     real b_Buybook;
     real b_Readstory;
     real b_Buybook_Readstory;
     real a0_Sex;
     real<lower=0> sigma_Sex;
     vector[NSex] u_Sex;
}
transformed parameters{
     // Transform parameters
     real theta_Readbook[Nobs];
     vector[NSex] a_Sex;
     // Varying intercepts definition
     for(k in 1:NSex) {
        a_Sex[k] = a0_Sex + u_Sex[k];
     }

     for (i in 1:Nobs) {
        theta_Readbook[i] = b_Buybook * Buybook[i] + b_Readstory * Readstory[i] + b_Buybook_Readstory * Buybook[i] * Readstory[i] + a_Sex[Sex[i]];
     }
}
model{
     // Priors
     b_Buybook ~ normal( 0, 10 );
     b_Readstory ~ normal( 0, 10 );
     b_Buybook_Readstory ~ normal( 0, 10 );
     a0_Sex ~ normal(0,10);
     sigma_Sex ~ normal(0,10);
     u_Sex ~ normal(0, sigma_Sex);

     // Likelihoods
     Readbook ~ binomial_logit(1, theta_Readbook);
}
generated quantities {
     // log-likelihood posterior
     vector[Nobs] log_lik_Readbook;
     for (i in 1:Nobs) {
       log_lik_Readbook[i] = bernoulli_logit_lpmf(Readbook[i] | theta_Readbook[i]);
     }
}"

datStem <- with(filtered,
            list(Nobs         = length(Readbook),
                 Buybook          = as.numeric(Buybook),
                 Readstory          = as.numeric(Readstory),
                 Readbook          = as.numeric(Readbook),
                 NSex = length(unique(Sex)),
                 Sex     	      = as.numeric(Sex)))

mstanReadbook <- rstan::stan(model_code = model_string, data = datStem,
        		warmup=2000 , iter = 4000, chains = 4)

exclude <- c("lp__","theta_Readbook")
par_names <- setdiff(dimnames(mstanReadbook)[[3]], exclude)
par_names <- c("b_Buybook", "b_Readstory", "b_Buybook_Readstory", "a0_Sex", "sigma_Sex")

postParams <- rstan::extract(mstanReadbook, pars = par_names)
bayesplot::color_scheme_set("blue")
posterior <- as.data.frame(mstanReadbook)
posterior <- posterior[par_names]
pm_2 <- bayesplot::mcmc_intervals(posterior,  point_est = "mean", prob = 0.8, prob_outer = 0.95)+theme_bw(base_size=15)+xlab("b)")
pm_2




#######################

model_string <- 
"data{
     // Define variables in data
     int<lower=1> Nobs;   // Number of observations (an integer)
     int<lower=0,upper=1> Readbook[Nobs];   // outcome variable
     int<lower=0,upper=1> Buybook[Nobs];
     int<lower=0,upper=1> Readstory[Nobs];
     int NSex;
     int<lower=1,upper=NSex> Sex[Nobs];
}
parameters{
     // Define parameters to estimate
     real b_Buybook;
     real b_Readstory;
     real a0_Sex;
     real<lower=0> sigma_Sex;
     vector[NSex] u_Sex;
}
transformed parameters{
     // Transform parameters
     real theta_Readbook[Nobs];
     vector[NSex] a_Sex;
     // Varying intercepts definition
     for(k in 1:NSex) {
        a_Sex[k] = a0_Sex + u_Sex[k];
     }

     for (i in 1:Nobs) {
        theta_Readbook[i] = b_Buybook * Buybook[i] + b_Readstory * Readstory[i] + a_Sex[Sex[i]];
     }
}
model{
     // Priors
     b_Buybook ~ normal( 0, 10 );
     b_Readstory ~ normal( 0, 10 );
     a0_Sex ~ normal(0,10);
     sigma_Sex ~ normal(0,10);
     u_Sex ~ normal(0, sigma_Sex);

     // Likelihoods
     Readbook ~ binomial_logit(1, theta_Readbook);
}
generated quantities {
     // log-likelihood posterior
     vector[Nobs] log_lik_Readbook;
     for (i in 1:Nobs) {
       log_lik_Readbook[i] = bernoulli_logit_lpmf(Readbook[i] | theta_Readbook[i]);
     }
}"

datStem <- with(filtered,
            list(Nobs         = length(Readbook),
                 Buybook          = as.numeric(Buybook),
                 Readstory          = as.numeric(Readstory),
                 Readbook          = as.numeric(Readbook),
                 NSex = length(unique(Sex)),
                 Sex     	      = as.numeric(Sex)))

mstanReadbook <- rstan::stan(model_code = model_string, data = datStem,
        		warmup=2000 , iter = 4000, chains = 4)

exclude <- c("lp__","theta_Readbook")
par_names <- setdiff(dimnames(mstanReadbook)[[3]], exclude)
par_names <- c("b_Buybook", "b_Readstory", "a0_Sex", "sigma_Sex")

postParams <- rstan::extract(mstanReadbook, pars = par_names)
bayesplot::color_scheme_set("blue")
posterior <- as.data.frame(mstanReadbook)
posterior <- posterior[par_names]
pm_2 <- bayesplot::mcmc_intervals(posterior,  point_est = "mean", prob = 0.8, prob_outer = 0.95)+theme_bw(base_size=15)+xlab("b)")
pm_2




#######################

model_string <- 
"data{
     // Define variables in data
     int<lower=1> Nobs;   // Number of observations (an integer)
     int<lower=0,upper=1> Readbook[Nobs];   // outcome variable
     int<lower=0,upper=1> Buybook[Nobs];
     int<lower=0,upper=1> Readstory[Nobs];
}
parameters{
     // Define parameters to estimate
     real b_Buybook;
     real b_Readstory;
}
transformed parameters{
     // Transform parameters
     real theta_Readbook[Nobs];

     for (i in 1:Nobs) {
        theta_Readbook[i] = b_Buybook * Buybook[i] + b_Readstory * Readstory[i];
     }
}
model{
     // Priors
     b_Buybook ~ normal( 0, 10 );
     b_Readstory ~ normal( 0, 10 );

     // Likelihoods
     Readbook ~ binomial_logit(1, theta_Readbook);
}
generated quantities {
     // log-likelihood posterior
     vector[Nobs] log_lik_Readbook;
     for (i in 1:Nobs) {
       log_lik_Readbook[i] = bernoulli_logit_lpmf(Readbook[i] | theta_Readbook[i]);
     }
}"

datStem <- with(filtered,
            list(Nobs         = length(Readbook),
                 Buybook          = as.numeric(Buybook),
                 Readstory          = as.numeric(Readstory),
                 Readbook          = as.numeric(Readbook)
								))

mstanReadbook1 <- rstan::stan(model_code = model_string, data = datStem,
        		warmup=2000 , iter = 4000, chains = 4)

exclude <- c("lp__","theta_Readbook")
par_names <- setdiff(dimnames(mstanReadbook1)[[3]], exclude)
par_names <- c("b_Buybook", "b_Readstory")

postParams <- rstan::extract(mstanReadbook1, pars = par_names)
bayesplot::color_scheme_set("blue")
posterior <- as.data.frame(mstanReadbook1)
posterior <- posterior[par_names]
pm_2 <- bayesplot::mcmc_intervals(posterior,  point_est = "mean", prob = 0.8, prob_outer = 0.95)+theme_bw(base_size=15)+xlab("b)")
pm_2

x <- data.frame(pr1=posterior$b_Buybook,pr2=posterior$b_Readstory)

library(ggplot2);
library(reshape2)
data<- melt(x)
listLabel <- c("Buybook", "Readstory")
p_3 <- ggplot(data,aes(x=value, color=variable)) + geom_density(size=1) + 
scale_color_discrete(labels = listLabel) +
theme_bw(base_size=15) +
theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), legend.title = element_blank(), legend.position = "bottom") 
p_3

################################

filtered <- dat[!is.na(dat$TimeSoc),]
filtered <- droplevels(filtered)

q1s <- unique(filtered[, "TimeSci"])
q2s <- unique(filtered[, "TimeSoc"])

x <- data.frame("TimeSci" = q1s)

for(q1 in q1s)
{	
	for(q2 in q2s)
	{
	  x[x$TimeSci==q1,as.character(q2)]<-length(which(filtered[,"TimeSci"]==q1 & filtered[,"TimeSoc"]==q2))
	}
}
