PITimeRule <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, configString = NULL, configStringStored = NULL, displayString = NULL, editorType = NULL, isConfigured = NULL, isInitializing = NULL, mergeDuplicatedItems = NULL, plugInName = NULL, links = NULL, webException = NULL) {
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
	if (is.null(configString) == FALSE) {
		if (is.character(configString) == FALSE) {
			return (print(paste0("Error: configString must be a string.")))
		}
	}
	if (is.null(configStringStored) == FALSE) {
		if (is.character(configStringStored) == FALSE) {
			return (print(paste0("Error: configStringStored must be a string.")))
		}
	}
	if (is.null(displayString) == FALSE) {
		if (is.character(displayString) == FALSE) {
			return (print(paste0("Error: displayString must be a string.")))
		}
	}
	if (is.null(editorType) == FALSE) {
		if (is.character(editorType) == FALSE) {
			return (print(paste0("Error: editorType must be a string.")))
		}
	}
	if (is.null(isConfigured) == FALSE) {
		if (is.logical(isConfigured) == FALSE) {
			return (print(paste0("Error: isConfigured must be a boolean.")))
		}
	}
	if (is.null(isInitializing) == FALSE) {
		if (is.logical(isInitializing) == FALSE) {
			return (print(paste0("Error: isInitializing must be a boolean.")))
		}
	}
	if (is.null(mergeDuplicatedItems) == FALSE) {
		if (is.logical(mergeDuplicatedItems) == FALSE) {
			return (print(paste0("Error: mergeDuplicatedItems must be a boolean.")))
		}
	}
	if (is.null(plugInName) == FALSE) {
		if (is.character(plugInName) == FALSE) {
			return (print(paste0("Error: plugInName must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PITimeRuleLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PITimeRuleLinks.")))
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
		ConfigString = configString,
		ConfigStringStored = configStringStored,
		DisplayString = displayString,
		EditorType = editorType,
		IsConfigured = isConfigured,
		IsInitializing = isInitializing,
		MergeDuplicatedItems = mergeDuplicatedItems,
		PlugInName = plugInName,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PITimeRule"
	return(valueCleaned)
}
