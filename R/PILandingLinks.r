PILandingLinks <- function(self = NULL, assetServers = NULL, dataServers = NULL, pIDirectory = NULL, search = NULL, system = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(assetServers) == FALSE) {
		if (is.character(assetServers) == FALSE) {
			return (print(paste0("Error: assetServers must be a string.")))
		}
	}
	if (is.null(dataServers) == FALSE) {
		if (is.character(dataServers) == FALSE) {
			return (print(paste0("Error: dataServers must be a string.")))
		}
	}
	if (is.null(pIDirectory) == FALSE) {
		if (is.character(pIDirectory) == FALSE) {
			return (print(paste0("Error: pIDirectory must be a string.")))
		}
	}
	if (is.null(search) == FALSE) {
		if (is.character(search) == FALSE) {
			return (print(paste0("Error: search must be a string.")))
		}
	}
	if (is.null(system) == FALSE) {
		if (is.character(system) == FALSE) {
			return (print(paste0("Error: system must be a string.")))
		}
	}
	value <- list(
		Self = self,
		AssetServers = assetServers,
		DataServers = dataServers,
		PIDirectory = pIDirectory,
		Search = search,
		System = system)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PILandingLinks"
	return(valueCleaned)
}
