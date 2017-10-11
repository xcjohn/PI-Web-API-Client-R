PIEnumerationSet <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, links = NULL, serializeDescription = NULL) {
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
	if (is.null(links) == FALSE) {
	}
	if (is.null(serializeDescription) == FALSE) {
		if (is.logical(serializeDescription) == FALSE) {
			return (print(paste0("Error: serializeDescription must be a boolean.")))
		}
	}
	value <- list(
	WebId = webId,
	Id = id,
	Name = name,
	Description = description,
	Path = path,
	Links = links,
	SerializeDescription = serializeDescription)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIEnumerationSet"
	return(valueCleaned)
}
