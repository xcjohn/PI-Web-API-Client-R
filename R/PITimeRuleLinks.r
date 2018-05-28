PITimeRuleLinks <- function(self = NULL, analysis = NULL, analysisTemplate = NULL, plugIn = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
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
	if (is.null(plugIn) == FALSE) {
		if (is.character(plugIn) == FALSE) {
			return (print(paste0("Error: plugIn must be a string.")))
		}
	}
	value <- list(
		Self = self,
		Analysis = analysis,
		AnalysisTemplate = analysisTemplate,
		PlugIn = plugIn)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PITimeRuleLinks"
	return(valueCleaned)
}
