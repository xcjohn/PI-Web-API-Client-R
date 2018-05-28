PITimedValue <- function(timestamp = NULL, unitsAbbreviation = NULL, good = NULL, questionable = NULL, substituted = NULL, annotated = NULL, value = NULL, errors = NULL, webException = NULL) {
	if (is.null(timestamp) == FALSE) {
		if (is.character(timestamp) == FALSE) {
			return (print(paste0("Error: timestamp must be a string.")))
		}
	}
	if (is.null(unitsAbbreviation) == FALSE) {
		if (is.character(unitsAbbreviation) == FALSE) {
			return (print(paste0("Error: unitsAbbreviation must be a string.")))
		}
	}
	if (is.null(good) == FALSE) {
		if (is.logical(good) == FALSE) {
			return (print(paste0("Error: good must be a boolean.")))
		}
	}
	if (is.null(questionable) == FALSE) {
		if (is.logical(questionable) == FALSE) {
			return (print(paste0("Error: questionable must be a boolean.")))
		}
	}
	if (is.null(substituted) == FALSE) {
		if (is.logical(substituted) == FALSE) {
			return (print(paste0("Error: substituted must be a boolean.")))
		}
	}
	if (is.null(annotated) == FALSE) {
		if (is.logical(annotated) == FALSE) {
			return (print(paste0("Error: annotated must be a boolean.")))
		}
	}
	if (is.null(value) == FALSE) {
	}
	if (is.null(errors) == FALSE) {
		if (is.vector(errors) == FALSE) {
			return (print(paste0("Error: errors must be a vector.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Timestamp = timestamp,
		UnitsAbbreviation = unitsAbbreviation,
		Good = good,
		Questionable = questionable,
		Substituted = substituted,
		Annotated = annotated,
		Value = value,
		Errors = errors,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PITimedValue"
	return(valueCleaned)
}
