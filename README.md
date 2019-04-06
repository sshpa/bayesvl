bayesvl
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

```sh
> install.packages("https://github.com/sshpa/baysvl/archive/master.zip")
```

## Create bayesian network structures

Create network node for each variable in the survey

```r
dag <- network_init()
dag <- network_addNode(dag, "B")
dag <- network_addNode(dag, "C")
dag <- network_addNode(dag, "T")
dag <- network_addNode(dag, "DC")
dag <- network_addNode(dag, "MD")
```

Start adding arcs between variables in the survey

```r
dag <- network_addArc(dag, "B", "DC")
dag <- network_addArc(dag, "C", "DC")
dag <- network_addArc(dag, "T", "DC")
```

## Generating STAN code

Generating the STAN code required for building structures of Bayesian networks for sampling and parameter learning

```r
stan_code <- stan_buildCode(dag, data)
stan_code
```

Get model's parameters

```r
params <- stan_params(dag, data)
params
```
