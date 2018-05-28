PIAttribute <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, type = NULL, typeQualifier = NULL, defaultUnitsName = NULL, displayDigits = NULL, dataReferencePlugIn = NULL, configString = NULL, isConfigurationItem = NULL, isExcluded = NULL, isHidden = NULL, isManualDataEntry = NULL, hasChildren = NULL, categoryNames = NULL, step = NULL, traitName = NULL, defaultUnitsNameAbbreviation = NULL, span = NULL, zero = NULL, links = NULL, webException = NULL) {
	if (is.null(webId) == FALSE) {
		if (is.character(webId) == FALSE) {
			return (print(paste0("Error: webId must be a string.")))
		}
	}
	if (is.null(id) == FALSE) {
		if (is.character(id) == FALSE) {
			return (print(paste0("Error: id must be a string.")))
		}
	}
	if (is.null(name) == FALSE) {
		if (is.character(name) == FALSE) {
			return (print(paste0("Error: name must be a string.")))
		}
	}
	if (is.null(description) == FALSE) {
		if (is.character(description) == FALSE) {
			return (print(paste0("Error: description must be a string.")))
		}
	}
	if (is.null(path) == FALSE) {
		if (is.character(path) == FALSE) {
			return (print(paste0("Error: path must be a string.")))
		}
	}
	if (is.null(type) == FALSE) {
		if (is.character(type) == FALSE) {
			return (print(paste0("Error: type must be a string.")))
		}
	}
	if (is.null(typeQualifier) == FALSE) {
		if (is.character(typeQualifier) == FALSE) {
			return (print(paste0("Error: typeQualifier must be a string.")))
		}
	}
	if (is.null(defaultUnitsName) == FALSE) {
		if (is.character(defaultUnitsName) == FALSE) {
			return (print(paste0("Error: defaultUnitsName must be a string.")))
		}
	}
	if (is.null(displayDigits) == FALSE) {
		if (check.integer(displayDigits) == FALSE) {
			return (print(paste0("Error: displayDigits must be an integer.")))
		}
	}
	if (is.null(dataReferencePlugIn) == FALSE) {
		if (is.character(dataReferencePlugIn) == FALSE) {
			return (print(paste0("Error: dataReferencePlugIn must be a string.")))
		}
	}
	if (is.null(configString) == FALSE) {
		if (is.character(configString) == FALSE) {
			return (print(paste0("Error: configString must be a string.")))
		}
	}
	if (is.null(isConfigurationItem) == FALSE) {
		if (is.logical(isConfigurationItem) == FALSE) {
			return (print(paste0("Error: isConfigurationItem must be a boolean.")))
		}
	}
	if (is.null(isExcluded) == FALSE) {
		if (is.logical(isExcluded) == FALSE) {
			return (print(paste0("Error: isExcluded must be a boolean.")))
		}
	}
	if (is.null(isHidden) == FALSE) {
		if (is.logical(isHidden) == FALSE) {
			return (print(paste0("Error: isHidden must be a boolean.")))
		}
	}
	if (is.null(isManualDataEntry) == FALSE) {
		if (is.logical(isManualDataEntry) == FALSE) {
			return (print(paste0("Error: isManualDataEntry must be a boolean.")))
		}
	}
	if (is.null(hasChildren) == FALSE) {
		if (is.logical(hasChildren) == FALSE) {
			return (print(paste0("Error: hasChildren must be a boolean.")))
		}
	}
	if (is.null(categoryNames) == FALSE) {
		if (is.vector(categoryNames) == FALSE) {
			return (print(paste0("Error: categoryNames must be a vector.")))
		}
		if (is.character(categoryNames[[1]]) == FALSE) {
			return (print(paste0("Error: categoryNames[[1]] must be a string.")))
		}
	}
	if (is.null(step) == FALSE) {
		if (is.logical(step) == FALSE) {
			return (print(paste0("Error: step must be a boolean.")))
		}
	}
	if (is.null(traitName) == FALSE) {
		if (is.character(traitName) == FALSE) {
			return (print(paste0("Error: traitName must be a string.")))
		}
	}
	if (is.null(defaultUnitsNameAbbreviation) == FALSE) {
		if (is.character(defaultUnitsNameAbbreviation) == FALSE) {
			return (print(paste0("Error: defaultUnitsNameAbbreviation must be a string.")))
		}
	}
	if (is.null(span) == FALSE) {
	}
	if (is.null(zero) == FALSE) {
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIAttributeLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIAttributeLinks.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		WebId = webId,
		Id = id,
		Name = name,
		Description = description,
		Path = path,
		Type = type,
		TypeQualifier = typeQualifier,
		DefaultUnitsName = defaultUnitsName,
		DisplayDigits = displayDigits,
		DataReferencePlugIn = dataReferencePlugIn,
		ConfigString = configString,
		IsConfigurationItem = isConfigurationItem,
		IsExcluded = isExcluded,
		IsHidden = isHidden,
		IsManualDataEntry = isManualDataEntry,
		HasChildren = hasChildren,
		CategoryNames = categoryNames,
		Step = step,
		TraitName = traitName,
		DefaultUnitsNameAbbreviation = defaultUnitsNameAbbreviation,
		Span = span,
		Zero = zero,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAttribute"
	return(valueCleaned)
}
