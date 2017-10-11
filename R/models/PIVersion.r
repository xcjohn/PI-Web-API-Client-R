PIVersion <- function(fullVersion = NULL, majorMinorRevision = NULL, build = NULL) {
	if (is.null(fullVersion) == FALSE) {
		if (is.character(fullVersion) == FALSE) {
			return (print(paste0("Error: fullVersion must be a string.")))
		}
	}
	if (is.null(majorMinorRevision) == FALSE) {
		if (is.character(majorMinorRevision) == FALSE) {
			return (print(paste0("Error: majorMinorRevision must be a string.")))
		}
	}
	if (is.null(build) == FALSE) {
		if (is.character(build) == FALSE) {
			return (print(paste0("Error: build must be a string.")))
		}
	}
	value <- list(
	FullVersion = fullVersion,
	MajorMinorRevision = majorMinorRevision,
	Build = build)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIVersion"
	return(valueCleaned)
}
