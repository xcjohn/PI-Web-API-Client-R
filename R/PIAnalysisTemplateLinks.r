PIAnalysisTemplateLinks <- function(self = NULL, database = NULL, categories = NULL, analysisRule = NULL, analysisRulePlugIn = NULL, timeRule = NULL, timeRulePlugIn = NULL, target = NULL, security = NULL, securityEntries = NULL) {
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
	if (is.null(categories) == FALSE) {
		if (is.character(categories) == FALSE) {
			return (print(paste0("Error: categories must be a string.")))
		}
	}
	if (is.null(analysisRule) == FALSE) {
		if (is.character(analysisRule) == FALSE) {
			return (print(paste0("Error: analysisRule must be a string.")))
		}
	}
	if (is.null(analysisRulePlugIn) == FALSE) {
		if (is.character(analysisRulePlugIn) == FALSE) {
			return (print(paste0("Error: analysisRulePlugIn must be a string.")))
		}
	}
	if (is.null(timeRule) == FALSE) {
		if (is.character(timeRule) == FALSE) {
			return (print(paste0("Error: timeRule must be a string.")))
		}
	}
	if (is.null(timeRulePlugIn) == FALSE) {
		if (is.character(timeRulePlugIn) == FALSE) {
			return (print(paste0("Error: timeRulePlugIn must be a string.")))
		}
	}
	if (is.null(target) == FALSE) {
		if (is.character(target) == FALSE) {
			return (print(paste0("Error: target must be a string.")))
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
		Categories = categories,
		AnalysisRule = analysisRule,
		AnalysisRulePlugIn = analysisRulePlugIn,
		TimeRule = timeRule,
		TimeRulePlugIn = timeRulePlugIn,
		Target = target,
		Security = security,
		SecurityEntries = securityEntries)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAnalysisTemplateLinks"
	return(valueCleaned)
}
