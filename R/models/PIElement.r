PIElement <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, templateName = NULL, hasChildren = NULL, categoryNames = NULL, extendedProperties = NULL, links = NULL) {
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
	if (is.null(templateName) == FALSE) {
		if (is.character(templateName) == FALSE) {
			return (print(paste0("Error: templateName must be a string.")))
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
	if (is.null(extendedProperties) == FALSE) {
	}
	if (is.null(links) == FALSE) {
	}
	value <- list(
	WebId = webId,
	Id = id,
	Name = name,
	Description = description,
	Path = path,
	TemplateName = templateName,
	HasChildren = hasChildren,
	CategoryNames = categoryNames,
	ExtendedProperties = extendedProperties,
	Links = links)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIElement"
	return(valueCleaned)
}
