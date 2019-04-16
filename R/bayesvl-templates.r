bvl2stan.templates <- list(
    Bern = list(
        name = "bern",
        stan_likelihood = "bernoulli",
        par_prefixes = c("theta"),
        par_bounds = c("<lower=0,upper=1>"),
        par_types = c("real"),
        out_type = "int<lower=0,upper=1>",
        par_map = function(k,e,...) {
            # get constraints and add <lower=0> for sigma vector
            constr_list <- get( "constraints" , envir=e )
            sigma_name <- as.character( k[[2]] )
            if ( is.null(constr_list[[sigma_name]]) ) {
                constr_list[[sigma_name]] <- "lower=0"
                assign( "constraints" , constr_list , envir=e )
            }
            return(k);
        },
        vectorized = TRUE
    ),
		Binomial = list(
        name = "binorm",
        par_prefixes = c("mu","sigma"),
        par_bounds = c("","<lower=0>"),
        par_types = c("real","real"),
        out_type = "int<lower=0,upper=1>",
        par_map = function(k,e,...) {
            # get constraints and add <lower=0> for sigma vector
            constr_list <- get( "constraints" , envir=e )
            sigma_name <- as.character( k[[2]] )
            if ( is.null(constr_list[[sigma_name]]) ) {
                constr_list[[sigma_name]] <- "lower=0"
                assign( "constraints" , constr_list , envir=e )
            }
            return(k);
        },
        vectorized = TRUE
    ),
    Normal = list(
        name = "norm",
        par_prefixes = c("mu","sigma"),
        par_bounds = c("","<lower=0>"),
        par_types = c("real","real"),
        out_type = "int<lower=0,upper=1>",
        par_map = function(k,e,...) {
            # get constraints and add <lower=0> for sigma vector
            constr_list <- get( "constraints" , envir=e )
            sigma_name <- as.character( k[[2]] )
            if ( is.null(constr_list[[sigma_name]]) ) {
                constr_list[[sigma_name]] <- "lower=0"
                assign( "constraints" , constr_list , envir=e )
            }
            return(k);
        },
        vectorized = TRUE
    )
)


bvl_loadTemplate <- function( fname ) {
    tmpname <- bvl_templateExists(fname)
    if ( is.na(tmpname) ) stop(concat("Distribution ",fname," not recognized."))
    return(templates[[ tmpname ]])
}

bvl_templateExists <- function( fname ) {
    the_match <- NA
    for ( i in 1:length(templates) ) {
        R_name <- templates[[i]][['name']]
        if ( fname %in% c(R_name) ) {
            the_match <- names(templates)[i]
            return(the_match)
        }
    }
    return(the_match)
}
