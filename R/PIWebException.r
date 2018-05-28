PIWebException <- function(statusCode = NULL, errors = NULL) {
	if (is.null(statusCode) == FALSE) {
		if (check.integer(statusCode) == FALSE) {
			return (print(paste0("Error: statusCode must be an integer.")))
		}
	}
	if (is.null(errors) == FALSE) {
		if (is.vector(errors) == FALSE) {
			return (print(paste0("Error: errors must be a vector.")))
		}
		if (is.character(errors[[1]]) == FALSE) {
			return (print(paste0("Error: errors[[1]] must be a string.")))
		}
	}
	value <- list(
		StatusCode = statusCode,
		Errors = errors)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIWebException"
	return(valueCleaned)
}
