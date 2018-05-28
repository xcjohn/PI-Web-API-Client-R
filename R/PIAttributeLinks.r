PIAttributeLinks <- function(self = NULL, attributes = NULL, element = NULL, eventFrame = NULL, parent = NULL, template = NULL, interpolatedData = NULL, recordedData = NULL, plotData = NULL, summaryData = NULL, value = NULL, endValue = NULL, point = NULL, categories = NULL, enumerationSet = NULL, enumerationValues = NULL, trait = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(attributes) == FALSE) {
		if (is.character(attributes) == FALSE) {
			return (print(paste0("Error: attributes must be a string.")))
		}
	}
	if (is.null(element) == FALSE) {
		if (is.character(element) == FALSE) {
			return (print(paste0("Error: element must be a string.")))
		}
	}
	if (is.null(eventFrame) == FALSE) {
		if (is.character(eventFrame) == FALSE) {
			return (print(paste0("Error: eventFrame must be a string.")))
		}
	}
	if (is.null(parent) == FALSE) {
		if (is.character(parent) == FALSE) {
			return (print(paste0("Error: parent must be a string.")))
		}
	}
	if (is.null(template) == FALSE) {
		if (is.character(template) == FALSE) {
			return (print(paste0("Error: template must be a string.")))
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
	if (is.null(point) == FALSE) {
		if (is.character(point) == FALSE) {
			return (print(paste0("Error: point must be a string.")))
		}
	}
	if (is.null(categories) == FALSE) {
		if (is.character(categories) == FALSE) {
			return (print(paste0("Error: categories must be a string.")))
		}
	}
	if (is.null(enumerationSet) == FALSE) {
		if (is.character(enumerationSet) == FALSE) {
			return (print(paste0("Error: enumerationSet must be a string.")))
		}
	}
	if (is.null(enumerationValues) == FALSE) {
		if (is.character(enumerationValues) == FALSE) {
			return (print(paste0("Error: enumerationValues must be a string.")))
		}
	}
	if (is.null(trait) == FALSE) {
		if (is.character(trait) == FALSE) {
			return (print(paste0("Error: trait must be a string.")))
		}
	}
	value <- list(
		Self = self,
		Attributes = attributes,
		Element = element,
		EventFrame = eventFrame,
		Parent = parent,
		Template = template,
		InterpolatedData = interpolatedData,
		RecordedData = recordedData,
		PlotData = plotData,
		SummaryData = summaryData,
		Value = value,
		EndValue = endValue,
		Point = point,
		Categories = categories,
		EnumerationSet = enumerationSet,
		EnumerationValues = enumerationValues,
		Trait = trait)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAttributeLinks"
	return(valueCleaned)
}
