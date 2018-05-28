PIAttributeTraitLinks <- function(self = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	value <- list(
		Self = self)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAttributeTraitLinks"
	return(valueCleaned)
}
