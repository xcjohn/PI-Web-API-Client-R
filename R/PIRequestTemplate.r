PIRequestTemplate <- function(resource = NULL) {
	if (is.null(resource) == FALSE) {
		if (is.character(resource) == FALSE) {
			return (print(paste0("Error: resource must be a string.")))
		}
	}
	value <- list(
		Resource = resource)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIRequestTemplate"
	return(valueCleaned)
}
