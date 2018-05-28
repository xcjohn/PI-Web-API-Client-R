PITimeRulePlugInLinks <- function(self = NULL, assetServer = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(assetServer) == FALSE) {
		if (is.character(assetServer) == FALSE) {
			return (print(paste0("Error: assetServer must be a string.")))
		}
	}
	value <- list(
		Self = self,
		AssetServer = assetServer)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PITimeRulePlugInLinks"
	return(valueCleaned)
}
