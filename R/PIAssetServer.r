PIAssetServer <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, isConnected = NULL, serverVersion = NULL, serverTime = NULL, extendedProperties = NULL, links = NULL, webException = NULL) {
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
	if (is.null(isConnected) == FALSE) {
		if (is.logical(isConnected) == FALSE) {
			return (print(paste0("Error: isConnected must be a boolean.")))
		}
	}
	if (is.null(serverVersion) == FALSE) {
		if (is.character(serverVersion) == FALSE) {
			return (print(paste0("Error: serverVersion must be a string.")))
		}
	}
	if (is.null(serverTime) == FALSE) {
		if (is.character(serverTime) == FALSE) {
			return (print(paste0("Error: serverTime must be a string.")))
		}
	}
	if (is.null(extendedProperties) == FALSE) {
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIAssetServerLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIAssetServerLinks.")))
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
		IsConnected = isConnected,
		ServerVersion = serverVersion,
		ServerTime = serverTime,
		ExtendedProperties = extendedProperties,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAssetServer"
	return(valueCleaned)
}
