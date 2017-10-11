PISystemStatus <- function(upTimeInMinutes = NULL, state = NULL, cacheInstances = NULL) {
	if (is.null(upTimeInMinutes) == FALSE) {
	}
	if (is.null(state) == FALSE) {
		if (is.character(state) == FALSE) {
			return (print(paste0("Error: state must be a string.")))
		}
	}
	if (is.null(cacheInstances) == FALSE) {
		if (check.integer(cacheInstances) == FALSE) {
			return (print(paste0("Error: cacheInstances must be an integer.")))
		}
	}
	value <- list(
	UpTimeInMinutes = upTimeInMinutes,
	State = state,
	CacheInstances = cacheInstances)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISystemStatus"
	return(valueCleaned)
}
