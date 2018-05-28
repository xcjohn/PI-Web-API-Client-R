PIValue <- function(value = NULL, exception = NULL, webException = NULL) {
	if (is.null(value) == FALSE) {
	}
	if (is.null(exception) == FALSE) {
		className <- attr(exception, "className")
		if ((is.null(className)) || (className != "PIErrors")) {
			return (print(paste0("Error: the class from the parameter exception should be PIErrors.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Value = value,
		Exception = exception,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIValue"
	return(valueCleaned)
}
