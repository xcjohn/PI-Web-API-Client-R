PIUnit <- function(webId = NULL, id = NULL, name = NULL, abbreviation = NULL, description = NULL, path = NULL, factor = NULL, offset = NULL, referenceFactor = NULL, referenceOffset = NULL, referenceUnitAbbreviation = NULL, links = NULL, webException = NULL) {
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
	if (is.null(abbreviation) == FALSE) {
		if (is.character(abbreviation) == FALSE) {
			return (print(paste0("Error: abbreviation must be a string.")))
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
	if (is.null(factor) == FALSE) {
	}
	if (is.null(offset) == FALSE) {
	}
	if (is.null(referenceFactor) == FALSE) {
	}
	if (is.null(referenceOffset) == FALSE) {
	}
	if (is.null(referenceUnitAbbreviation) == FALSE) {
		if (is.character(referenceUnitAbbreviation) == FALSE) {
			return (print(paste0("Error: referenceUnitAbbreviation must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIUnitLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIUnitLinks.")))
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
		Abbreviation = abbreviation,
		Description = description,
		Path = path,
		Factor = factor,
		Offset = offset,
		ReferenceFactor = referenceFactor,
		ReferenceOffset = referenceOffset,
		ReferenceUnitAbbreviation = referenceUnitAbbreviation,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIUnit"
	return(valueCleaned)
}
