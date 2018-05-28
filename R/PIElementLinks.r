PIElementLinks <- function(self = NULL, analyses = NULL, attributes = NULL, elements = NULL, database = NULL, parent = NULL, template = NULL, categories = NULL, defaultAttribute = NULL, eventFrames = NULL, interpolatedData = NULL, recordedData = NULL, plotData = NULL, summaryData = NULL, value = NULL, endValue = NULL, security = NULL, securityEntries = NULL, notificationRules = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(analyses) == FALSE) {
		if (is.character(analyses) == FALSE) {
			return (print(paste0("Error: analyses must be a string.")))
		}
	}
	if (is.null(attributes) == FALSE) {
		if (is.character(attributes) == FALSE) {
			return (print(paste0("Error: attributes must be a string.")))
		}
	}
	if (is.null(elements) == FALSE) {
		if (is.character(elements) == FALSE) {
			return (print(paste0("Error: elements must be a string.")))
		}
	}
	if (is.null(database) == FALSE) {
		if (is.character(database) == FALSE) {
			return (print(paste0("Error: database must be a string.")))
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
	if (is.null(categories) == FALSE) {
		if (is.character(categories) == FALSE) {
			return (print(paste0("Error: categories must be a string.")))
		}
	}
	if (is.null(defaultAttribute) == FALSE) {
		if (is.character(defaultAttribute) == FALSE) {
			return (print(paste0("Error: defaultAttribute must be a string.")))
		}
	}
	if (is.null(eventFrames) == FALSE) {
		if (is.character(eventFrames) == FALSE) {
			return (print(paste0("Error: eventFrames must be a string.")))
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
	if (is.null(notificationRules) == FALSE) {
		if (is.character(notificationRules) == FALSE) {
			return (print(paste0("Error: notificationRules must be a string.")))
		}
	}
	value <- list(
		Self = self,
		Analyses = analyses,
		Attributes = attributes,
		Elements = elements,
		Database = database,
		Parent = parent,
		Template = template,
		Categories = categories,
		DefaultAttribute = defaultAttribute,
		EventFrames = eventFrames,
		InterpolatedData = interpolatedData,
		RecordedData = recordedData,
		PlotData = plotData,
		SummaryData = summaryData,
		Value = value,
		EndValue = endValue,
		Security = security,
		SecurityEntries = securityEntries,
		NotificationRules = notificationRules)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIElementLinks"
	return(valueCleaned)
}
