PIAttributeTemplateLinks <- function(self = NULL, attributeTemplates = NULL, elementTemplate = NULL, parent = NULL, categories = NULL, trait = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(attributeTemplates) == FALSE) {
		if (is.character(attributeTemplates) == FALSE) {
			return (print(paste0("Error: attributeTemplates must be a string.")))
		}
	}
	if (is.null(elementTemplate) == FALSE) {
		if (is.character(elementTemplate) == FALSE) {
			return (print(paste0("Error: elementTemplate must be a string.")))
		}
	}
	if (is.null(parent) == FALSE) {
		if (is.character(parent) == FALSE) {
			return (print(paste0("Error: parent must be a string.")))
		}
	}
	if (is.null(categories) == FALSE) {
		if (is.character(categories) == FALSE) {
			return (print(paste0("Error: categories must be a string.")))
		}
	}
	if (is.null(trait) == FALSE) {
		if (is.character(trait) == FALSE) {
			return (print(paste0("Error: trait must be a string.")))
		}
	}
	value <- list(
		Self = self,
		AttributeTemplates = attributeTemplates,
		ElementTemplate = elementTemplate,
		Parent = parent,
		Categories = categories,
		Trait = trait)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAttributeTemplateLinks"
	return(valueCleaned)
}
