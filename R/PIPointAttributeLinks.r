PIPointAttributeLinks <- function(self = NULL, point = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(point) == FALSE) {
		if (is.character(point) == FALSE) {
			return (print(paste0("Error: point must be a string.")))
		}
	}
	value <- list(
		Self = self,
		Point = point)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIPointAttributeLinks"
	return(valueCleaned)
}
