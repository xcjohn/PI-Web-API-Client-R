PISecurity <- function(canAnnotate = NULL, canDelete = NULL, canExecute = NULL, canRead = NULL, canReadData = NULL, canSubscribe = NULL, canSubscribeOthers = NULL, canWrite = NULL, canWriteData = NULL, hasAdmin = NULL, rights = NULL, webException = NULL) {
	if (is.null(canAnnotate) == FALSE) {
		if (is.logical(canAnnotate) == FALSE) {
			return (print(paste0("Error: canAnnotate must be a boolean.")))
		}
	}
	if (is.null(canDelete) == FALSE) {
		if (is.logical(canDelete) == FALSE) {
			return (print(paste0("Error: canDelete must be a boolean.")))
		}
	}
	if (is.null(canExecute) == FALSE) {
		if (is.logical(canExecute) == FALSE) {
			return (print(paste0("Error: canExecute must be a boolean.")))
		}
	}
	if (is.null(canRead) == FALSE) {
		if (is.logical(canRead) == FALSE) {
			return (print(paste0("Error: canRead must be a boolean.")))
		}
	}
	if (is.null(canReadData) == FALSE) {
		if (is.logical(canReadData) == FALSE) {
			return (print(paste0("Error: canReadData must be a boolean.")))
		}
	}
	if (is.null(canSubscribe) == FALSE) {
		if (is.logical(canSubscribe) == FALSE) {
			return (print(paste0("Error: canSubscribe must be a boolean.")))
		}
	}
	if (is.null(canSubscribeOthers) == FALSE) {
		if (is.logical(canSubscribeOthers) == FALSE) {
			return (print(paste0("Error: canSubscribeOthers must be a boolean.")))
		}
	}
	if (is.null(canWrite) == FALSE) {
		if (is.logical(canWrite) == FALSE) {
			return (print(paste0("Error: canWrite must be a boolean.")))
		}
	}
	if (is.null(canWriteData) == FALSE) {
		if (is.logical(canWriteData) == FALSE) {
			return (print(paste0("Error: canWriteData must be a boolean.")))
		}
	}
	if (is.null(hasAdmin) == FALSE) {
		if (is.logical(hasAdmin) == FALSE) {
			return (print(paste0("Error: hasAdmin must be a boolean.")))
		}
	}
	if (is.null(rights) == FALSE) {
		if (is.vector(rights) == FALSE) {
			return (print(paste0("Error: rights must be a vector.")))
		}
		if (is.character(rights[[1]]) == FALSE) {
			return (print(paste0("Error: rights[[1]] must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		CanAnnotate = canAnnotate,
		CanDelete = canDelete,
		CanExecute = canExecute,
		CanRead = canRead,
		CanReadData = canReadData,
		CanSubscribe = canSubscribe,
		CanSubscribeOthers = canSubscribeOthers,
		CanWrite = canWrite,
		CanWriteData = canWriteData,
		HasAdmin = hasAdmin,
		Rights = rights,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISecurity"
	return(valueCleaned)
}
