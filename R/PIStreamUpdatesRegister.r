PIStreamUpdatesRegister <- function(status = NULL, source = NULL, sourceName = NULL, sourcePath = NULL, latestMarker = NULL, exception = NULL) {
	if (is.null(status) == FALSE) {
		if (is.character(status) == FALSE) {
			return (print(paste0("Error: status must be a string.")))
		}
	}
	if (is.null(source) == FALSE) {
		if (is.character(source) == FALSE) {
			return (print(paste0("Error: source must be a string.")))
		}
	}
	if (is.null(sourceName) == FALSE) {
		if (is.character(sourceName) == FALSE) {
			return (print(paste0("Error: sourceName must be a string.")))
		}
	}
	if (is.null(sourcePath) == FALSE) {
		if (is.character(sourcePath) == FALSE) {
			return (print(paste0("Error: sourcePath must be a string.")))
		}
	}
	if (is.null(latestMarker) == FALSE) {
		if (is.character(latestMarker) == FALSE) {
			return (print(paste0("Error: latestMarker must be a string.")))
		}
	}
	if (is.null(exception) == FALSE) {
		className <- attr(exception, "className")
		if ((is.null(className)) || (className != "PIErrors")) {
			return (print(paste0("Error: the class from the parameter exception should be PIErrors.")))
		}
	}
	value <- list(
		Status = status,
		Source = source,
		SourceName = sourceName,
		SourcePath = sourcePath,
		LatestMarker = latestMarker,
		Exception = exception)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIStreamUpdatesRegister"
	return(valueCleaned)
}
