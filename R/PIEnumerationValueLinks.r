PIEnumerationValueLinks <- function(self = NULL, enumerationSet = NULL, parent = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(enumerationSet) == FALSE) {
		if (is.character(enumerationSet) == FALSE) {
			return (print(paste0("Error: enumerationSet must be a string.")))
		}
	}
	if (is.null(parent) == FALSE) {
		if (is.character(parent) == FALSE) {
			return (print(paste0("Error: parent must be a string.")))
		}
	}
	value <- list(
		Self = self,
		EnumerationSet = enumerationSet,
		Parent = parent)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIEnumerationValueLinks"
	return(valueCleaned)
}
