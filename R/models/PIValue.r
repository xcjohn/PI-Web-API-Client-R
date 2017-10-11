PIValue <- function(value = NULL, exception = NULL) {
	if (is.null(value) == FALSE) {
	}
	if (is.null(exception) == FALSE) {
		className <- attr(exception, "className")
		if ((is.null(className)) || (className != "PIErrors")) {
			return (print(paste0("Error: the class from the parameter exception should be PIErrors.")))
		}
	}
	value <- list(
	Value = value,
	Exception = exception)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIValue"
	return(valueCleaned)
}
