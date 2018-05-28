PITimedValues <- function(items = NULL, unitsAbbreviation = NULL, webException = NULL) {
	if (is.null(items) == FALSE) {
		if (is.vector(items) == FALSE) {
			return (print(paste0("Error: items must be a vector.")))
		}
	}
	if (is.null(unitsAbbreviation) == FALSE) {
		if (is.character(unitsAbbreviation) == FALSE) {
			return (print(paste0("Error: unitsAbbreviation must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Items = items,
		UnitsAbbreviation = unitsAbbreviation,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PITimedValues"
	return(valueCleaned)
}
