PIStreamAnnotation <- function(id = NULL, name = NULL, description = NULL, value = NULL, creator = NULL, creationDate = NULL, modifier = NULL, modifyDate = NULL, webException = NULL) {
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
	}
	if (is.null(creator) == FALSE) {
		if (is.character(creator) == FALSE) {
			return (print(paste0("Error: creator must be a string.")))
		}
	}
	if (is.null(creationDate) == FALSE) {
		if (is.character(creationDate) == FALSE) {
			return (print(paste0("Error: creationDate must be a string.")))
		}
	}
	if (is.null(modifier) == FALSE) {
		if (is.character(modifier) == FALSE) {
			return (print(paste0("Error: modifier must be a string.")))
		}
	}
	if (is.null(modifyDate) == FALSE) {
		if (is.character(modifyDate) == FALSE) {
			return (print(paste0("Error: modifyDate must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Id = id,
		Name = name,
		Description = description,
		Value = value,
		Creator = creator,
		CreationDate = creationDate,
		Modifier = modifier,
		ModifyDate = modifyDate,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIStreamAnnotation"
	return(valueCleaned)
}
