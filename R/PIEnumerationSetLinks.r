PIEnumerationSetLinks <- function(self = NULL, database = NULL, dataServer = NULL, values = NULL, security = NULL, securityEntries = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(database) == FALSE) {
		if (is.character(database) == FALSE) {
			return (print(paste0("Error: database must be a string.")))
		}
	}
	if (is.null(dataServer) == FALSE) {
		if (is.character(dataServer) == FALSE) {
			return (print(paste0("Error: dataServer must be a string.")))
		}
	}
	if (is.null(values) == FALSE) {
		if (is.character(values) == FALSE) {
			return (print(paste0("Error: values must be a string.")))
		}
	}
	if (is.null(security) == FALSE) {
		if (is.character(security) == FALSE) {
			return (print(paste0("Error: security must be a string.")))
		}
	}
	if (is.null(securityEntries) == FALSE) {
		if (is.character(securityEntries) == FALSE) {
			return (print(paste0("Error: securityEntries must be a string.")))
		}
	}
	value <- list(
		Self = self,
		Database = database,
		DataServer = dataServer,
		Values = values,
		Security = security,
		SecurityEntries = securityEntries)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIEnumerationSetLinks"
	return(valueCleaned)
}
