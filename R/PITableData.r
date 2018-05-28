PITableData <- function(columns = NULL, rows = NULL, webException = NULL) {
	if (is.null(columns) == FALSE) {
	}
	if (is.null(rows) == FALSE) {
		if (is.vector(rows) == FALSE) {
			return (print(paste0("Error: rows must be a vector.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Columns = columns,
		Rows = rows,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PITableData"
	return(valueCleaned)
}
