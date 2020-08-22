BayesVL
==========

The R package for visually learning the graphical structures of Bayesian networks, and performing Hamiltonian Markov chain Monte Carlo (MCMC) with 'Stan'.

Features:

    * Creating the (starting) graphical structure of Bayesian networks
    * Creating one or more random Bayesian networks learned from dataset with customized constraints
    * Generating Stan code for structures of Bayesian networks for sampling and parameter learning
    * Plotting the Bayesian network graphs 
    * Performing Markov chain Monte Carlo (MCMC) simulations and plotting various graphs for posteriors check
    * Compatibility with R 3.4 or newer versions

Here is the [CHANGELOG](https://github.com/sshpa/baysvl/blob/master/CHANGELOG.md)

# Get started

## Installation
### Prerequisites
You'll need to install ``rstan`` first. Stan’s website ``http://mc-stan.org`` provides up-to-date information, follow the instructions for your platform. For instructions on installing a C++ compiler for use with RStan see ``https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started`` are quite thorough.

### Latest Release

Getting started and installing the latest snapshot type in the R console:

```r
> install.packages(c("coda","devtools","loo","ggplot2"))
> devtools::install_github("sshpa/bayesvl")
```

# Example

## Create appropriate Bayesian network structures

Creating a node for each variable in the proposed network

```r
dag <- bayesvl()
dag <- bvl_addNode(dag, "Burden", "norm")
dag <- bvl_addNode(dag, "Res", "norm")
dag <- bvl_addNode(dag, "Insured", "norm")
```

Starting to add arcs between variables (nodes) using the survey data

```r
dag <- bvl_addArc(dag, "Res", "Burden", "slope")
dag <- bvl_addArc(dag, "Insured", "Burden", "slope")
```

## Generate 'Stan' code

Generating the 'Stan' code required for building structures of the Bayesian networks required for sampling and parameter learning

```r
stan_code <- bvl_model2Stan(dag)
cat(stan_code)
```

Getting the model's parameters

```r
params <- bvl_stanParams(dag)
params
```

## Preparing the data

We used the ``data1042`` data set that is built-in to the package.

```r
# load data
data( data1042 )
```

## Sample and fit the 'Stan' model

Sampling the predefined 'Stan' model

```r
stan_fit <- bvl_modelFit(dag, data1042, iter=20000 , warmup=2000 , chains=4 , cores=4)
summary(stan_fit)
```

