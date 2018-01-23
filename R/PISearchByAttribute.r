PISearchByAttribute <- function(searchRoot = NULL, elementTemplate = NULL, items = NULL, webException = NULL) {
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
	if (is.null(items) == FALSE) {
		if (is.vector(items) == FALSE) {
			return (print(paste0("Error: items must be a vector.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
	SearchRoot = searchRoot,
	ElementTemplate = elementTemplate,
	Items = items,
	WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISearchByAttribute"
	return(valueCleaned)
}
