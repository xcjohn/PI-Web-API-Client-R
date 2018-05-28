PIPaginationLinks <- function(first = NULL, previous = NULL, last = NULL) {
	if (is.null(first) == FALSE) {
		if (is.character(first) == FALSE) {
			return (print(paste0("Error: first must be a string.")))
		}
	}
	if (is.null(previous) == FALSE) {
		if (is.character(previous) == FALSE) {
			return (print(paste0("Error: previous must be a string.")))
		}
	}
	if (is.null(last) == FALSE) {
		if (is.character(last) == FALSE) {
			return (print(paste0("Error: last must be a string.")))
		}
	}
	value <- list(
		First = first,
		Previous = previous,
		Last = last)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIPaginationLinks"
	return(valueCleaned)
}
