PIPointLinks <- function(self = NULL, dataServer = NULL, attributes = NULL, interpolatedData = NULL, recordedData = NULL, plotData = NULL, summaryData = NULL, value = NULL, endValue = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(dataServer) == FALSE) {
		if (is.character(dataServer) == FALSE) {
			return (print(paste0("Error: dataServer must be a string.")))
		}
	}
	if (is.null(attributes) == FALSE) {
		if (is.character(attributes) == FALSE) {
			return (print(paste0("Error: attributes must be a string.")))
		}
	}
	if (is.null(interpolatedData) == FALSE) {
		if (is.character(interpolatedData) == FALSE) {
			return (print(paste0("Error: interpolatedData must be a string.")))
		}
	}
	if (is.null(recordedData) == FALSE) {
		if (is.character(recordedData) == FALSE) {
			return (print(paste0("Error: recordedData must be a string.")))
		}
	}
	if (is.null(plotData) == FALSE) {
		if (is.character(plotData) == FALSE) {
			return (print(paste0("Error: plotData must be a string.")))
		}
	}
	if (is.null(summaryData) == FALSE) {
		if (is.character(summaryData) == FALSE) {
			return (print(paste0("Error: summaryData must be a string.")))
		}
	}
	if (is.null(value) == FALSE) {
		if (is.character(value) == FALSE) {
			return (print(paste0("Error: value must be a string.")))
		}
	}
	if (is.null(endValue) == FALSE) {
		if (is.character(endValue) == FALSE) {
			return (print(paste0("Error: endValue must be a string.")))
		}
	}
	value <- list(
		Self = self,
		DataServer = dataServer,
		Attributes = attributes,
		InterpolatedData = interpolatedData,
		RecordedData = recordedData,
		PlotData = plotData,
		SummaryData = summaryData,
		Value = value,
		EndValue = endValue)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIPointLinks"
	return(valueCleaned)
}
