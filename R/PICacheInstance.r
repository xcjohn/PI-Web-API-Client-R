PICacheInstance <- function(id = NULL, lastRefreshTime = NULL, willRefreshAfter = NULL, scheduledExpirationTime = NULL, user = NULL, webException = NULL) {
	if (is.null(id) == FALSE) {
		if (is.character(id) == FALSE) {
			return (print(paste0("Error: id must be a string.")))
		}
	}
	if (is.null(lastRefreshTime) == FALSE) {
		if (is.character(lastRefreshTime) == FALSE) {
			return (print(paste0("Error: lastRefreshTime must be a string.")))
		}
	}
	if (is.null(willRefreshAfter) == FALSE) {
		if (is.character(willRefreshAfter) == FALSE) {
			return (print(paste0("Error: willRefreshAfter must be a string.")))
		}
	}
	if (is.null(scheduledExpirationTime) == FALSE) {
		if (is.character(scheduledExpirationTime) == FALSE) {
			return (print(paste0("Error: scheduledExpirationTime must be a string.")))
		}
	}
	if (is.null(user) == FALSE) {
		if (is.character(user) == FALSE) {
			return (print(paste0("Error: user must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Id = id,
		LastRefreshTime = lastRefreshTime,
		WillRefreshAfter = willRefreshAfter,
		ScheduledExpirationTime = scheduledExpirationTime,
		User = user,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PICacheInstance"
	return(valueCleaned)
}
