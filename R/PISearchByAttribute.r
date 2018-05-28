PISearchByAttribute <- function(searchRoot = NULL, elementTemplate = NULL, webException = NULL, valueQueries = NULL) {
	if (is.null(searchRoot) == FALSE) {
		if (is.character(searchRoot) == FALSE) {
			return (print(paste0("Error: searchRoot must be a string.")))
		}
	}
	if (is.null(elementTemplate) == FALSE) {
		if (is.character(elementTemplate) == FALSE) {
			return (print(paste0("Error: elementTemplate must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	if (is.null(valueQueries) == FALSE) {
		if (is.vector(valueQueries) == FALSE) {
			return (print(paste0("Error: valueQueries must be a vector.")))
		}
	}
	value <- list(
		SearchRoot = searchRoot,
		ElementTemplate = elementTemplate,
		WebException = webException,
		ValueQueries = valueQueries)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISearchByAttribute"
	return(valueCleaned)
}
