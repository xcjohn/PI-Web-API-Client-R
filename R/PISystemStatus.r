PISystemStatus <- function(upTimeInMinutes = NULL, state = NULL, cacheInstances = NULL, serverTime = NULL, webException = NULL) {
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
	if (is.null(serverTime) == FALSE) {
		if (is.character(serverTime) == FALSE) {
			return (print(paste0("Error: serverTime must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		UpTimeInMinutes = upTimeInMinutes,
		State = state,
		CacheInstances = cacheInstances,
		ServerTime = serverTime,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISystemStatus"
	return(valueCleaned)
}
