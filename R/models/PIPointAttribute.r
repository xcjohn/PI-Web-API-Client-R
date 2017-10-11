PIPointAttribute <- function(name = NULL, value = NULL, links = NULL) {
	if (is.null(name) == FALSE) {
		if (is.character(name) == FALSE) {
			return (print(paste0("Error: name must be a string.")))
		}
	}
	if (is.null(value) == FALSE) {
	}
	if (is.null(links) == FALSE) {
	}
	value <- list(
	Name = name,
	Value = value,
	Links = links)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIPointAttribute"
	return(valueCleaned)
}
