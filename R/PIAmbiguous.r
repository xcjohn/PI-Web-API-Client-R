PIAmbiguous <- function(reason = NULL) {
	if (is.null(reason) == FALSE) {
		if (is.character(reason) == FALSE) {
			return (print(paste0("Error: reason must be a string.")))
		}
	}
	value <- list(
		Reason = reason)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAmbiguous"
	return(valueCleaned)
}
