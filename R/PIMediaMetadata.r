PIMediaMetadata <- function(author = NULL, changeDate = NULL, description = NULL, name = NULL, size = NULL, links = NULL, webException = NULL) {
	if (is.null(author) == FALSE) {
		if (is.character(author) == FALSE) {
			return (print(paste0("Error: author must be a string.")))
		}
	}
	if (is.null(changeDate) == FALSE) {
		if (is.character(changeDate) == FALSE) {
			return (print(paste0("Error: changeDate must be a string.")))
		}
	}
	if (is.null(description) == FALSE) {
		if (is.character(description) == FALSE) {
			return (print(paste0("Error: description must be a string.")))
		}
	}
	if (is.null(name) == FALSE) {
		if (is.character(name) == FALSE) {
			return (print(paste0("Error: name must be a string.")))
		}
	}
	if (is.null(size) == FALSE) {
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIMediaMetadataLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIMediaMetadataLinks.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Author = author,
		ChangeDate = changeDate,
		Description = description,
		Name = name,
		Size = size,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIMediaMetadata"
	return(valueCleaned)
}
