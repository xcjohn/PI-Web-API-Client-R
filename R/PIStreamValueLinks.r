PIStreamValueLinks <- function(source = NULL) {
	if (is.null(source) == FALSE) {
		if (is.character(source) == FALSE) {
			return (print(paste0("Error: source must be a string.")))
		}
	}
	value <- list(
		Source = source)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIStreamValueLinks"
	return(valueCleaned)
}
