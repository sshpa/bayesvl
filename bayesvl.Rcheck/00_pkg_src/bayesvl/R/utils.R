is.empty <- function(object) {
	if (class(object) == "NULL")
		return (TRUE)	

	if (is.null(object))
		return (TRUE)
		
	if (length(object) == 0)
		return (TRUE)
	
	return(FALSE)
} 
