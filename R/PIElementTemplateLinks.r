PIElementTemplateLinks <- function(self = NULL, analysisTemplates = NULL, attributeTemplates = NULL, database = NULL, categories = NULL, baseTemplate = NULL, baseTemplates = NULL, derivedTemplates = NULL, defaultAttribute = NULL, notificationRuleTemplates = NULL, security = NULL, securityEntries = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(analysisTemplates) == FALSE) {
		if (is.character(analysisTemplates) == FALSE) {
			return (print(paste0("Error: analysisTemplates must be a string.")))
		}
	}
	if (is.null(attributeTemplates) == FALSE) {
		if (is.character(attributeTemplates) == FALSE) {
			return (print(paste0("Error: attributeTemplates must be a string.")))
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
	if (is.null(baseTemplate) == FALSE) {
		if (is.character(baseTemplate) == FALSE) {
			return (print(paste0("Error: baseTemplate must be a string.")))
		}
	}
	if (is.null(baseTemplates) == FALSE) {
		if (is.character(baseTemplates) == FALSE) {
			return (print(paste0("Error: baseTemplates must be a string.")))
		}
	}
	if (is.null(derivedTemplates) == FALSE) {
		if (is.character(derivedTemplates) == FALSE) {
			return (print(paste0("Error: derivedTemplates must be a string.")))
		}
	}
	if (is.null(defaultAttribute) == FALSE) {
		if (is.character(defaultAttribute) == FALSE) {
			return (print(paste0("Error: defaultAttribute must be a string.")))
		}
	}
	if (is.null(notificationRuleTemplates) == FALSE) {
		if (is.character(notificationRuleTemplates) == FALSE) {
			return (print(paste0("Error: notificationRuleTemplates must be a string.")))
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
		AnalysisTemplates = analysisTemplates,
		AttributeTemplates = attributeTemplates,
		Database = database,
		Categories = categories,
		BaseTemplate = baseTemplate,
		BaseTemplates = baseTemplates,
		DerivedTemplates = derivedTemplates,
		DefaultAttribute = defaultAttribute,
		NotificationRuleTemplates = notificationRuleTemplates,
		Security = security,
		SecurityEntries = securityEntries)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIElementTemplateLinks"
	return(valueCleaned)
}
