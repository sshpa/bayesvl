is.empty <- function(object) {
	if (class(object) == "NULL")
		return (FALSE)	

	if (is.null(object))
		return (FALSE)
		
	if (length(object) == 0)
		return (FALSE)
	
	return(TRUE)
} 
