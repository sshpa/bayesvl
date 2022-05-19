bayesvl ChangeLog
-------------------------
1.0.0
 * update lot functions
 * add PPC

0.9.0
 * add WAIC estimation functions
 * add Loo 2.0 estimation functions
 * add model comparing functions
 * updated document .Rd and other information

0.8.5
 * updated document .Rd and other information
 * fix bugs for CRAN submission

0.7.6
 * fixed error of single node model
 * updated document .Rd and other information

0.7.0
 * fixed alpha intercept for varying intercept model
 * fixed lower=0 for varying intercept model
 * change file net2stan.r to bayesvl2stan.r
 * added WAIC calculation
 
0.6.8
 * added arc templates
 * added validate model functions
 * added auto generate data list for Stan estimation
 * added compare log_lik function

0.6.5
 * support node type Dummy for temporary parameters
 * support node type Trans for transformed data
 * support user's 'generated quantities' block
 * support y_rep
 * support log_lik
 * update readme.md

0.6.0
 * added more distribution templates
 * updated stan code generator from network graph
 * update readme.md

0.5.1
 * lots of documentation updates.

0.5.0
 * added functions for stan code generating
 * added distribution templates
 * update readme.md

0.3.0
 * added bnPlot(), bnScore(), bnStrength() to call bnlearn functions
 * added utitlites for convert between bayesvl network structure and bnlearn network structure
 * update readme.md

0.2.0
 * added functions for add/remove node to network graph 
 * added functions for add/remove arc between variable nodes
 * initialize network function
 * implemented object bayesvl class
 * first completely documented release

0.1.0
 * package description & other information
 * initial release
