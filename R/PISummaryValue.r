PISummaryValue <- function(type = NULL, value = NULL, webException = NULL) {
	if (is.null(type) == FALSE) {
		if (is.character(type) == FALSE) {
			return (print(paste0("Error: type must be a string.")))
		}
	}
	if (is.null(value) == FALSE) {
		className <- attr(value, "className")
		if ((is.null(className)) || (className != "PITimedValue")) {
			return (print(paste0("Error: the class from the parameter value should be PITimedValue.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Type = type,
		Value = value,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISummaryValue"
	return(valueCleaned)
}
