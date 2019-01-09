bayesvl
==========

A R package for visually learning the graphical structure of Bayesian networks.

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
install.packages("https://github.com/sshpa/baysvl/archive/master.zip")
```

## Create bayesian network structures

Create network node for each variable in data

```r
dag <- bayesvl.network_init()
dag <- bayesvl.network_addNode(dag, "B")
dag <- bayesvl.network_addNode(dag, "C")
dag <- bayesvl.network_addNode(dag, "T")
dag <- bayesvl.network_addNode(dag, "DC")
dag <- bayesvl.network_addNode(dag, "MD")
```
