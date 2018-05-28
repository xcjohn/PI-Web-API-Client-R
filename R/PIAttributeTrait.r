PIAttributeTrait <- function(name = NULL, abbreviation = NULL, allowChildAttributes = NULL, allowDuplicates = NULL, isAllowedOnRootAttribute = NULL, isTypeInherited = NULL, isUOMInherited = NULL, requireNumeric = NULL, requireString = NULL, links = NULL, webException = NULL) {
	if (is.null(name) == FALSE) {
		if (is.character(name) == FALSE) {
			return (print(paste0("Error: name must be a string.")))
		}
	}
	if (is.null(abbreviation) == FALSE) {
		if (is.character(abbreviation) == FALSE) {
			return (print(paste0("Error: abbreviation must be a string.")))
		}
	}
	if (is.null(allowChildAttributes) == FALSE) {
		if (is.logical(allowChildAttributes) == FALSE) {
			return (print(paste0("Error: allowChildAttributes must be a boolean.")))
		}
	}
	if (is.null(allowDuplicates) == FALSE) {
		if (is.logical(allowDuplicates) == FALSE) {
			return (print(paste0("Error: allowDuplicates must be a boolean.")))
		}
	}
	if (is.null(isAllowedOnRootAttribute) == FALSE) {
		if (is.logical(isAllowedOnRootAttribute) == FALSE) {
			return (print(paste0("Error: isAllowedOnRootAttribute must be a boolean.")))
		}
	}
	if (is.null(isTypeInherited) == FALSE) {
		if (is.logical(isTypeInherited) == FALSE) {
			return (print(paste0("Error: isTypeInherited must be a boolean.")))
		}
	}
	if (is.null(isUOMInherited) == FALSE) {
		if (is.logical(isUOMInherited) == FALSE) {
			return (print(paste0("Error: isUOMInherited must be a boolean.")))
		}
	}
	if (is.null(requireNumeric) == FALSE) {
		if (is.logical(requireNumeric) == FALSE) {
			return (print(paste0("Error: requireNumeric must be a boolean.")))
		}
	}
	if (is.null(requireString) == FALSE) {
		if (is.logical(requireString) == FALSE) {
			return (print(paste0("Error: requireString must be a boolean.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIAttributeTraitLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIAttributeTraitLinks.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Name = name,
		Abbreviation = abbreviation,
		AllowChildAttributes = allowChildAttributes,
		AllowDuplicates = allowDuplicates,
		IsAllowedOnRootAttribute = isAllowedOnRootAttribute,
		IsTypeInherited = isTypeInherited,
		IsUOMInherited = isUOMInherited,
		RequireNumeric = requireNumeric,
		RequireString = requireString,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAttributeTrait"
	return(valueCleaned)
}
