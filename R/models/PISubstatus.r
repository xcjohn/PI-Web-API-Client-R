PISubstatus <- function(substatus = NULL, message = NULL) {
	if (is.null(substatus) == FALSE) {
		if (check.integer(substatus) == FALSE) {
			return (print(paste0("Error: substatus must be an integer.")))
		}
	}
	if (is.null(message) == FALSE) {
		if (is.character(message) == FALSE) {
			return (print(paste0("Error: message must be a string.")))
		}
	}
	value <- list(
	Substatus = substatus,
	Message = message)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISubstatus"
	return(valueCleaned)
}
