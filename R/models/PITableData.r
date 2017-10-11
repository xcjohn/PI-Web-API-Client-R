PITableData <- function(columns = NULL, rows = NULL) {
	if (is.null(columns) == FALSE) {
	}
	if (is.null(rows) == FALSE) {
		if (is.vector(rows) == FALSE) {
			return (print(paste0("Error: rows must be a vector.")))
		}
	}
	value <- list(
	Columns = columns,
	Rows = rows)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PITableData"
	return(valueCleaned)
}
