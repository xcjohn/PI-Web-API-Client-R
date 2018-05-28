PISystemLandingLinks <- function(self = NULL, cacheInstances = NULL, configuration = NULL, userInfo = NULL, versions = NULL, status = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(cacheInstances) == FALSE) {
		if (is.character(cacheInstances) == FALSE) {
			return (print(paste0("Error: cacheInstances must be a string.")))
		}
	}
	if (is.null(configuration) == FALSE) {
		if (is.character(configuration) == FALSE) {
			return (print(paste0("Error: configuration must be a string.")))
		}
	}
	if (is.null(userInfo) == FALSE) {
		if (is.character(userInfo) == FALSE) {
			return (print(paste0("Error: userInfo must be a string.")))
		}
	}
	if (is.null(versions) == FALSE) {
		if (is.character(versions) == FALSE) {
			return (print(paste0("Error: versions must be a string.")))
		}
	}
	if (is.null(status) == FALSE) {
		if (is.character(status) == FALSE) {
			return (print(paste0("Error: status must be a string.")))
		}
	}
	value <- list(
		Self = self,
		CacheInstances = cacheInstances,
		Configuration = configuration,
		UserInfo = userInfo,
		Versions = versions,
		Status = status)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISystemLandingLinks"
	return(valueCleaned)
}
