PIUnitLinks <- function(self = NULL, class = NULL, referenceUnit = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(class) == FALSE) {
		if (is.character(class) == FALSE) {
			return (print(paste0("Error: class must be a string.")))
		}
	}
	if (is.null(referenceUnit) == FALSE) {
		if (is.character(referenceUnit) == FALSE) {
			return (print(paste0("Error: referenceUnit must be a string.")))
		}
	}
	value <- list(
		Self = self,
		Class = class,
		ReferenceUnit = referenceUnit)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIUnitLinks"
	return(valueCleaned)
}
