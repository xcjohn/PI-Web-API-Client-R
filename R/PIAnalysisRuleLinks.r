PIAnalysisRuleLinks <- function(self = NULL, analysisRules = NULL, analysis = NULL, analysisTemplate = NULL, parent = NULL, plugIn = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(analysisRules) == FALSE) {
		if (is.character(analysisRules) == FALSE) {
			return (print(paste0("Error: analysisRules must be a string.")))
		}
	}
	if (is.null(analysis) == FALSE) {
		if (is.character(analysis) == FALSE) {
			return (print(paste0("Error: analysis must be a string.")))
		}
	}
	if (is.null(analysisTemplate) == FALSE) {
		if (is.character(analysisTemplate) == FALSE) {
			return (print(paste0("Error: analysisTemplate must be a string.")))
		}
	}
	if (is.null(parent) == FALSE) {
		if (is.character(parent) == FALSE) {
			return (print(paste0("Error: parent must be a string.")))
		}
	}
	if (is.null(plugIn) == FALSE) {
		if (is.character(plugIn) == FALSE) {
			return (print(paste0("Error: plugIn must be a string.")))
		}
	}
	value <- list(
		Self = self,
		AnalysisRules = analysisRules,
		Analysis = analysis,
		AnalysisTemplate = analysisTemplate,
		Parent = parent,
		PlugIn = plugIn)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAnalysisRuleLinks"
	return(valueCleaned)
}
