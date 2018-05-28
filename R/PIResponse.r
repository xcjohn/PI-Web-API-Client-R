PIResponse <- function(status = NULL, headers = NULL, content = NULL) {
	if (is.null(status) == FALSE) {
		if (check.integer(status) == FALSE) {
			return (print(paste0("Error: status must be an integer.")))
		}
	}
	if (is.null(headers) == FALSE) {
	}
	if (is.null(content) == FALSE) {
	}
	value <- list(
		Status = status,
		Headers = headers,
		Content = content)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIResponse"
	return(valueCleaned)
}
