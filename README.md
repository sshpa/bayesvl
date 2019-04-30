BayesVL
==========

The R package for visually learning the graphical structures of Bayesian networks.

Features:

    * Creating the (starting) graphical structure of Bayesian networks
    * Creating one or more random Bayesian networks learned from dataset with customized constraints
    * Generating JAGS/STAN code for structures of Bayesian networks for sampling and parameter learning
    * Plotting the network graphs and MCMC
    * Compatible with R 2.x or newer version

Here is the [CHANGELOG](https://github.com/sshpa/baysvl/blob/master/CHANGELOG.md)

# Get started

## Install

To get started abd install the latest snapshot type in R console:

```r
> install.packages("devtools")
> devtools::install_github("sshpa/bayesvl")
```

## Create bayesian network structures

Create network node for each variable in the survey

```r
dag <- bayesvl()
dag <- bvl_addNode(dag, "B")
dag <- bvl_addNode(dag, "C")
dag <- bvl_addNode(dag, "T")
dag <- bvl_addNode(dag, "DC")
dag <- bvl_addNode(dag, "MD")
```

Start adding arcs between variables in the survey

```r
dag <- bvl_addArc(dag, "B", "DC")
dag <- bvl_addArc(dag, "C", "DC")
dag <- bvl_addArc(dag, "T", "DC")
```

## Generate STAN code

Generating the STAN code required for building structures of Bayesian networks for sampling and parameter learning

```r
stan_code <- bvl_model2Stan(dag)
cat(stan_code)
```

Get model's parameters

```r
params <- bvl_stanParams(dag)
params
```
## Sample and fit the STAN model

Sampling the STAN model

```r
stan_fit <- bvl_modelFit(dag, data, iter=20000 , warmup=2000 , chains=4 , cores=4)
summary(stan_fit)
```

