PIStreamSummaries <- function(webId = NULL, name = NULL, path = NULL, items = NULL, links = NULL) {
	if (is.null(webId) == FALSE) {
		if (is.character(webId) == FALSE) {
			return (print(paste0("Error: webId must be a string.")))
		}
	}
	if (is.null(name) == FALSE) {
		if (is.character(name) == FALSE) {
			return (print(paste0("Error: name must be a string.")))
		}
	}
	if (is.null(path) == FALSE) {
		if (is.character(path) == FALSE) {
			return (print(paste0("Error: path must be a string.")))
		}
	}
	if (is.null(items) == FALSE) {
		if (is.vector(items) == FALSE) {
			return (print(paste0("Error: items must be a vector.")))
		}
	}
	if (is.null(links) == FALSE) {
	}
	value <- list(
	WebId = webId,
	Name = name,
	Path = path,
	Items = items,
	Links = links)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIStreamSummaries"
	return(valueCleaned)
}
