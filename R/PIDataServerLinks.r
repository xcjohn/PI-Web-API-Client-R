PIDataServerLinks <- function(self = NULL, points = NULL, enumerationSets = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(points) == FALSE) {
		if (is.character(points) == FALSE) {
			return (print(paste0("Error: points must be a string.")))
		}
	}
	if (is.null(enumerationSets) == FALSE) {
		if (is.character(enumerationSets) == FALSE) {
			return (print(paste0("Error: enumerationSets must be a string.")))
		}
	}
	value <- list(
		Self = self,
		Points = points,
		EnumerationSets = enumerationSets)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIDataServerLinks"
	return(valueCleaned)
}
