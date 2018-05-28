PITimeRulePlugIn <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, assemblyFileName = NULL, assemblyID = NULL, assemblyLoadProperties = NULL, assemblyTime = NULL, compatibilityVersion = NULL, isBrowsable = NULL, isNonEditableConfig = NULL, loadedAssemblyTime = NULL, loadedVersion = NULL, version = NULL, links = NULL, webException = NULL) {
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
	if (is.null(assemblyFileName) == FALSE) {
		if (is.character(assemblyFileName) == FALSE) {
			return (print(paste0("Error: assemblyFileName must be a string.")))
		}
	}
	if (is.null(assemblyID) == FALSE) {
		if (is.character(assemblyID) == FALSE) {
			return (print(paste0("Error: assemblyID must be a string.")))
		}
	}
	if (is.null(assemblyLoadProperties) == FALSE) {
		if (is.vector(assemblyLoadProperties) == FALSE) {
			return (print(paste0("Error: assemblyLoadProperties must be a vector.")))
		}
		if (is.character(assemblyLoadProperties[[1]]) == FALSE) {
			return (print(paste0("Error: assemblyLoadProperties[[1]] must be a string.")))
		}
	}
	if (is.null(assemblyTime) == FALSE) {
		if (is.character(assemblyTime) == FALSE) {
			return (print(paste0("Error: assemblyTime must be a string.")))
		}
	}
	if (is.null(compatibilityVersion) == FALSE) {
		if (check.integer(compatibilityVersion) == FALSE) {
			return (print(paste0("Error: compatibilityVersion must be an integer.")))
		}
	}
	if (is.null(isBrowsable) == FALSE) {
		if (is.logical(isBrowsable) == FALSE) {
			return (print(paste0("Error: isBrowsable must be a boolean.")))
		}
	}
	if (is.null(isNonEditableConfig) == FALSE) {
		if (is.logical(isNonEditableConfig) == FALSE) {
			return (print(paste0("Error: isNonEditableConfig must be a boolean.")))
		}
	}
	if (is.null(loadedAssemblyTime) == FALSE) {
		if (is.character(loadedAssemblyTime) == FALSE) {
			return (print(paste0("Error: loadedAssemblyTime must be a string.")))
		}
	}
	if (is.null(loadedVersion) == FALSE) {
		if (is.character(loadedVersion) == FALSE) {
			return (print(paste0("Error: loadedVersion must be a string.")))
		}
	}
	if (is.null(version) == FALSE) {
		if (is.character(version) == FALSE) {
			return (print(paste0("Error: version must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PITimeRulePlugInLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PITimeRulePlugInLinks.")))
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
		AssemblyFileName = assemblyFileName,
		AssemblyID = assemblyID,
		AssemblyLoadProperties = assemblyLoadProperties,
		AssemblyTime = assemblyTime,
		CompatibilityVersion = compatibilityVersion,
		IsBrowsable = isBrowsable,
		IsNonEditableConfig = isNonEditableConfig,
		LoadedAssemblyTime = loadedAssemblyTime,
		LoadedVersion = loadedVersion,
		Version = version,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PITimeRulePlugIn"
	return(valueCleaned)
}
