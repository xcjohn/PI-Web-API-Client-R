PISubstatus <- function(substatus = NULL, message = NULL, webException = NULL) {
	if (is.null(substatus) == FALSE) {
		if (check.integer(substatus) == FALSE) {
			return (print(paste0("Error: substatus must be an integer.")))
		}
	}
	if (is.null(message) == FALSE) {
		if (is.character(message) == FALSE) {
			return (print(paste0("Error: message must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Substatus = substatus,
		Message = message,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISubstatus"
	return(valueCleaned)
}
