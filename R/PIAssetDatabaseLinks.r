PIAssetDatabaseLinks <- function(self = NULL, elements = NULL, elementTemplates = NULL, eventFrames = NULL, assetServer = NULL, elementCategories = NULL, attributeCategories = NULL, tableCategories = NULL, analysisCategories = NULL, analysisTemplates = NULL, enumerationSets = NULL, tables = NULL, security = NULL, securityEntries = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(elements) == FALSE) {
		if (is.character(elements) == FALSE) {
			return (print(paste0("Error: elements must be a string.")))
		}
	}
	if (is.null(elementTemplates) == FALSE) {
		if (is.character(elementTemplates) == FALSE) {
			return (print(paste0("Error: elementTemplates must be a string.")))
		}
	}
	if (is.null(eventFrames) == FALSE) {
		if (is.character(eventFrames) == FALSE) {
			return (print(paste0("Error: eventFrames must be a string.")))
		}
	}
	if (is.null(assetServer) == FALSE) {
		if (is.character(assetServer) == FALSE) {
			return (print(paste0("Error: assetServer must be a string.")))
		}
	}
	if (is.null(elementCategories) == FALSE) {
		if (is.character(elementCategories) == FALSE) {
			return (print(paste0("Error: elementCategories must be a string.")))
		}
	}
	if (is.null(attributeCategories) == FALSE) {
		if (is.character(attributeCategories) == FALSE) {
			return (print(paste0("Error: attributeCategories must be a string.")))
		}
	}
	if (is.null(tableCategories) == FALSE) {
		if (is.character(tableCategories) == FALSE) {
			return (print(paste0("Error: tableCategories must be a string.")))
		}
	}
	if (is.null(analysisCategories) == FALSE) {
		if (is.character(analysisCategories) == FALSE) {
			return (print(paste0("Error: analysisCategories must be a string.")))
		}
	}
	if (is.null(analysisTemplates) == FALSE) {
		if (is.character(analysisTemplates) == FALSE) {
			return (print(paste0("Error: analysisTemplates must be a string.")))
		}
	}
	if (is.null(enumerationSets) == FALSE) {
		if (is.character(enumerationSets) == FALSE) {
			return (print(paste0("Error: enumerationSets must be a string.")))
		}
	}
	if (is.null(tables) == FALSE) {
		if (is.character(tables) == FALSE) {
			return (print(paste0("Error: tables must be a string.")))
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
		Elements = elements,
		ElementTemplates = elementTemplates,
		EventFrames = eventFrames,
		AssetServer = assetServer,
		ElementCategories = elementCategories,
		AttributeCategories = attributeCategories,
		TableCategories = tableCategories,
		AnalysisCategories = analysisCategories,
		AnalysisTemplates = analysisTemplates,
		EnumerationSets = enumerationSets,
		Tables = tables,
		Security = security,
		SecurityEntries = securityEntries)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAssetDatabaseLinks"
	return(valueCleaned)
}
