PIUnitClassLinks <- function(self = NULL, canonicalUnit = NULL, units = NULL, assetServer = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(canonicalUnit) == FALSE) {
		if (is.character(canonicalUnit) == FALSE) {
			return (print(paste0("Error: canonicalUnit must be a string.")))
		}
	}
	if (is.null(units) == FALSE) {
		if (is.character(units) == FALSE) {
			return (print(paste0("Error: units must be a string.")))
		}
	}
	if (is.null(assetServer) == FALSE) {
		if (is.character(assetServer) == FALSE) {
			return (print(paste0("Error: assetServer must be a string.")))
		}
	}
	value <- list(
		Self = self,
		CanonicalUnit = canonicalUnit,
		Units = units,
		AssetServer = assetServer)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIUnitClassLinks"
	return(valueCleaned)
}
