PIEnumerationValue <- function(webId = NULL, id = NULL, name = NULL, description = NULL, value = NULL, path = NULL, parent = NULL, links = NULL, serializeWebId = NULL, serializeId = NULL, serializeDescription = NULL, serializePath = NULL, serializeLinks = NULL, webException = NULL) {
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
	if (is.null(value) == FALSE) {
		if (check.integer(value) == FALSE) {
			return (print(paste0("Error: value must be an integer.")))
		}
	}
	if (is.null(path) == FALSE) {
		if (is.character(path) == FALSE) {
			return (print(paste0("Error: path must be a string.")))
		}
	}
	if (is.null(parent) == FALSE) {
		if (is.character(parent) == FALSE) {
			return (print(paste0("Error: parent must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIEnumerationValueLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIEnumerationValueLinks.")))
		}
	}
	if (is.null(serializeWebId) == FALSE) {
		if (is.logical(serializeWebId) == FALSE) {
			return (print(paste0("Error: serializeWebId must be a boolean.")))
		}
	}
	if (is.null(serializeId) == FALSE) {
		if (is.logical(serializeId) == FALSE) {
			return (print(paste0("Error: serializeId must be a boolean.")))
		}
	}
	if (is.null(serializeDescription) == FALSE) {
		if (is.logical(serializeDescription) == FALSE) {
			return (print(paste0("Error: serializeDescription must be a boolean.")))
		}
	}
	if (is.null(serializePath) == FALSE) {
		if (is.logical(serializePath) == FALSE) {
			return (print(paste0("Error: serializePath must be a boolean.")))
		}
	}
	if (is.null(serializeLinks) == FALSE) {
		if (is.logical(serializeLinks) == FALSE) {
			return (print(paste0("Error: serializeLinks must be a boolean.")))
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
		Value = value,
		Path = path,
		Parent = parent,
		Links = links,
		SerializeWebId = serializeWebId,
		SerializeId = serializeId,
		SerializeDescription = serializeDescription,
		SerializePath = serializePath,
		SerializeLinks = serializeLinks,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIEnumerationValue"
	return(valueCleaned)
}
