PIElementTemplate <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, allowElementToExtend = NULL, baseTemplate = NULL, instanceType = NULL, namingPattern = NULL, categoryNames = NULL, extendedProperties = NULL, severity = NULL, canBeAcknowledged = NULL, links = NULL, webException = NULL) {
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
	if (is.null(allowElementToExtend) == FALSE) {
		if (is.logical(allowElementToExtend) == FALSE) {
			return (print(paste0("Error: allowElementToExtend must be a boolean.")))
		}
	}
	if (is.null(baseTemplate) == FALSE) {
		if (is.character(baseTemplate) == FALSE) {
			return (print(paste0("Error: baseTemplate must be a string.")))
		}
	}
	if (is.null(instanceType) == FALSE) {
		if (is.character(instanceType) == FALSE) {
			return (print(paste0("Error: instanceType must be a string.")))
		}
	}
	if (is.null(namingPattern) == FALSE) {
		if (is.character(namingPattern) == FALSE) {
			return (print(paste0("Error: namingPattern must be a string.")))
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
	if (is.null(severity) == FALSE) {
		if (is.character(severity) == FALSE) {
			return (print(paste0("Error: severity must be a string.")))
		}
	}
	if (is.null(canBeAcknowledged) == FALSE) {
		if (is.logical(canBeAcknowledged) == FALSE) {
			return (print(paste0("Error: canBeAcknowledged must be a boolean.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIElementTemplateLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIElementTemplateLinks.")))
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
		AllowElementToExtend = allowElementToExtend,
		BaseTemplate = baseTemplate,
		InstanceType = instanceType,
		NamingPattern = namingPattern,
		CategoryNames = categoryNames,
		ExtendedProperties = extendedProperties,
		Severity = severity,
		CanBeAcknowledged = canBeAcknowledged,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIElementTemplate"
	return(valueCleaned)
}
