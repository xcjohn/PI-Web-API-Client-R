PIPropertyError <- function(fieldName = NULL, message = NULL) {
	if (is.null(fieldName) == FALSE) {
		if (is.character(fieldName) == FALSE) {
			return (print(paste0("Error: fieldName must be a string.")))
		}
	}
	if (is.null(message) == FALSE) {
		if (is.vector(message) == FALSE) {
			return (print(paste0("Error: message must be a vector.")))
		}
		if (is.character(message[[1]]) == FALSE) {
			return (print(paste0("Error: message[[1]] must be a string.")))
		}
	}
	value <- list(
		FieldName = fieldName,
		Message = message)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIPropertyError"
	return(valueCleaned)
}
