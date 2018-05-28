PIAssetServerLinks <- function(self = NULL, databases = NULL, notificationContactTemplates = NULL, securityIdentities = NULL, securityMappings = NULL, unitClasses = NULL, analysisRulePlugIns = NULL, timeRulePlugIns = NULL, security = NULL, securityEntries = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(databases) == FALSE) {
		if (is.character(databases) == FALSE) {
			return (print(paste0("Error: databases must be a string.")))
		}
	}
	if (is.null(notificationContactTemplates) == FALSE) {
		if (is.character(notificationContactTemplates) == FALSE) {
			return (print(paste0("Error: notificationContactTemplates must be a string.")))
		}
	}
	if (is.null(securityIdentities) == FALSE) {
		if (is.character(securityIdentities) == FALSE) {
			return (print(paste0("Error: securityIdentities must be a string.")))
		}
	}
	if (is.null(securityMappings) == FALSE) {
		if (is.character(securityMappings) == FALSE) {
			return (print(paste0("Error: securityMappings must be a string.")))
		}
	}
	if (is.null(unitClasses) == FALSE) {
		if (is.character(unitClasses) == FALSE) {
			return (print(paste0("Error: unitClasses must be a string.")))
		}
	}
	if (is.null(analysisRulePlugIns) == FALSE) {
		if (is.character(analysisRulePlugIns) == FALSE) {
			return (print(paste0("Error: analysisRulePlugIns must be a string.")))
		}
	}
	if (is.null(timeRulePlugIns) == FALSE) {
		if (is.character(timeRulePlugIns) == FALSE) {
			return (print(paste0("Error: timeRulePlugIns must be a string.")))
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
		Databases = databases,
		NotificationContactTemplates = notificationContactTemplates,
		SecurityIdentities = securityIdentities,
		SecurityMappings = securityMappings,
		UnitClasses = unitClasses,
		AnalysisRulePlugIns = analysisRulePlugIns,
		TimeRulePlugIns = timeRulePlugIns,
		Security = security,
		SecurityEntries = securityEntries)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAssetServerLinks"
	return(valueCleaned)
}
