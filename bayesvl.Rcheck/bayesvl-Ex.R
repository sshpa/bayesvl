pkgname <- "bayesvl"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "bayesvl-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('bayesvl')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Legends345")
### * Legends345

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Legends345
### Title: Legends345 data
### Aliases: Legends345
### Keywords: legends

### ** Examples

	data(Legends345)
	
	data1 <- Legends345
	head(data1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Legends345", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bayesvl-class")
### * bayesvl-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bayesvl-class
### Title: Class 'bayesvl': object class of bayesvl model
### Aliases: bayesvl-class show,bayesvl-method summary,bayesvl-method
###   bvl_addNode,bayesvl-method bvl_addArc,bayesvl-method
###   bvl_modelFit,bayesvl-method bvl_stanParams,bayesvl-method
### Keywords: classes

### ** Examples

	# Design the model in directed acyclic graph
	model <- bayesvl()
	
	# add observed data nodes to the model
	model <- bvl_addNode(model, "Lie", "binom")
	model <- bvl_addNode(model, "B", "binom")
	model <- bvl_addNode(model, "C", "binom")
	model <- bvl_addNode(model, "T", "binom")
	
	# add path between nodes
	model <- bvl_addArc(model, "B", "Lie", "slope")
	model <- bvl_addArc(model, "C", "Lie", "slope")
	model <- bvl_addArc(model, "T", "Lie", "slope")
  
  summary(model)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bayesvl-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bayesvl")
### * bayesvl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bayesvl-package
### Title: BayesVL package for Bayesian statistical analyses in R
### Aliases: bayesvl-package bayesvl
### Keywords: bayesvl package

### ** Examples

	# Design the model in directed acyclic graph
	model <- bayesvl()
	
	# add observed data nodes to the model
	model <- bvl_addNode(model, "Lie", "binom")
	model <- bvl_addNode(model, "B", "binom")
	model <- bvl_addNode(model, "C", "binom")
	model <- bvl_addNode(model, "T", "binom")
	
	# add path between nodes
	model <- bvl_addArc(model, "B", "Lie", "slope")
	model <- bvl_addArc(model, "C", "Lie", "slope")
	model <- bvl_addArc(model, "T", "Lie", "slope")
  
  summary(model)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bayesvl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("graphs")
### * graphs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bayesvl graph utilities
### Title: Utilities to manipulate graphs
### Aliases: 'bayesvl graph utilities' 'bayesvl graphs' bvl_addNode
###   bvl_addArc
### Keywords: directed acyclic graphs, bayesvl

### ** Examples


dag = bayesvl()

# add nodes to dag
dag = bvl_addNode(dag, "node1")
dag = bvl_addNode(dag, "node2")

# add the path between two nodes
dag = bvl_addArc(dag, "node1", "node2")

summary(dag)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("graphs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plots")
### * plots

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bayesvl plot utilities
### Title: Plot utilities for bayesvl objects
### Aliases: 'bayesvl plot utilities' 'bayesvl plots' bvl_plotParams
###   bvl_plotIntervals bvl_plotAreas bvl_plotPairs bvl_plotDensity
###   bvl_plotDensity2d bvl_plotTrace bvl_plotGelman bvl_plotGelmans
###   bvl_plotAcfs bvl_plotTest bvl_plotDiag bvl_bnPlot
### Keywords: directed acyclic graphs, bayesvl, bayesvl plots

### ** Examples


## create network model
model <- bayesvl()
## add the observed data nodes
model <- bvl_addNode(model, "O", "binom")
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "Viol", "binom")
model <- bvl_addNode(model, "VB", "binom")
model <- bvl_addNode(model, "VC", "binom")
model <- bvl_addNode(model, "VT", "binom")
model <- bvl_addNode(model, "Int1", "binom")
model <- bvl_addNode(model, "Int2", "binom")

## add the tranform data nodes and arcs as part of the model
model <- bvl_addNode(model, "B_and_Viol", "trans")
model <- bvl_addNode(model, "C_and_Viol", "trans")
model <- bvl_addNode(model, "T_and_Viol", "trans")
model <- bvl_addArc(model, "VB",        "B_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "B_and_Viol", "*")
model <- bvl_addArc(model, "VC",        "C_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "C_and_Viol", "*")
model <- bvl_addArc(model, "VT",        "T_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "T_and_Viol", "*")
model <- bvl_addArc(model, "B_and_Viol",  "O", "slope")
model <- bvl_addArc(model, "C_and_Viol",  "O", "slope")
model <- bvl_addArc(model, "T_and_Viol",  "O", "slope")

model <- bvl_addArc(model, "Viol",   "O", "slope")

model <- bvl_addNode(model, "B_and_Lie", "trans")
model <- bvl_addNode(model, "C_and_Lie", "trans")
model <- bvl_addNode(model, "T_and_Lie", "trans")
model <- bvl_addArc(model, "VB",       "B_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "B_and_Lie", "*")
model <- bvl_addArc(model, "VC",       "C_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "C_and_Lie", "*")
model <- bvl_addArc(model, "VT",       "T_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "T_and_Lie", "*")
model <- bvl_addArc(model, "B_and_Lie",  "O", "slope")
model <- bvl_addArc(model, "C_and_Lie",  "O", "slope")
model <- bvl_addArc(model, "T_and_Lie",  "O", "slope")

model <- bvl_addArc(model, "Lie",   "O", "slope")

model <- bvl_addNode(model, "Int1_or_Int2", "trans")
model <- bvl_addArc(model, "Int1", "Int1_or_Int2", "+")
model <- bvl_addArc(model, "Int2", "Int1_or_Int2", "+")

model <- bvl_addArc(model, "Int1_or_Int2", "O", "varint")

## Plot network diagram to visualize the model
bvl_bnPlot(model)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plots", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("stan")
### * stan

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bayesvl stan utilities
### Title: Build RStan models from directed acyclic graph
### Aliases: 'bayesvl stan' 'bayesvl stan utilities' bvl_model2Stan
###   bvl_modelFit bvl_stanPriors bvl_stanParams bvl_formula
### Keywords: directed acyclic graphs, bayesvl

### ** Examples


# Design the model in directed acyclic graph
model <- bayesvl()
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "B", "binom")
model <- bvl_addNode(model, "C", "binom")
model <- bvl_addNode(model, "T", "binom")

model <- bvl_addArc(model, "B", "Lie", "slope")
model <- bvl_addArc(model, "C", "Lie", "slope")
model <- bvl_addArc(model, "T", "Lie", "slope")

# Generate the Stan model's code
model_string <- bvl_model2Stan(model)
cat(model_string)

# Show priors in generated Stan model
bvl_stanPriors(model)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("stan", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
