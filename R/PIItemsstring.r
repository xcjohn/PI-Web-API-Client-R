PIItemsstring <- function(items = NULL) {
	if (is.null(items) == FALSE) {
		if (is.vector(items) == FALSE) {
			return (print(paste0("Error: items must be a vector.")))
		}
		if (is.character(items[[1]]) == FALSE) {
			return (print(paste0("Error: items[[1]] must be a string.")))
		}
	}
	value <- list(
		Items = items)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIItemsstring"
	return(valueCleaned)
}
