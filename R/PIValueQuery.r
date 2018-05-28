PIValueQuery <- function(attributeName = NULL, attributeUOM = NULL, attributeValue = NULL, searchOperator = NULL, webException = NULL) {
	if (is.null(attributeName) == FALSE) {
		if (is.character(attributeName) == FALSE) {
			return (print(paste0("Error: attributeName must be a string.")))
		}
	}
	if (is.null(attributeUOM) == FALSE) {
		if (is.character(attributeUOM) == FALSE) {
			return (print(paste0("Error: attributeUOM must be a string.")))
		}
	}
	if (is.null(attributeValue) == FALSE) {
	}
	if (is.null(searchOperator) == FALSE) {
		if (is.character(searchOperator) == FALSE) {
			return (print(paste0("Error: searchOperator must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		AttributeName = attributeName,
		AttributeUOM = attributeUOM,
		AttributeValue = attributeValue,
		SearchOperator = searchOperator,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIValueQuery"
	return(valueCleaned)
}
