PIRequest <- function(method = NULL, resource = NULL, requestTemplate = NULL, parameters = NULL, headers = NULL, content = NULL, parentIds = NULL) {
	if (is.null(method) == FALSE) {
		if (is.character(method) == FALSE) {
			return (print(paste0("Error: method must be a string.")))
		}
	}
	if (is.null(resource) == FALSE) {
		if (is.character(resource) == FALSE) {
			return (print(paste0("Error: resource must be a string.")))
		}
	}
	if (is.null(requestTemplate) == FALSE) {
		className <- attr(requestTemplate, "className")
		if ((is.null(className)) || (className != "PIRequestTemplate")) {
			return (print(paste0("Error: the class from the parameter requestTemplate should be PIRequestTemplate.")))
		}
	}
	if (is.null(parameters) == FALSE) {
		if (is.vector(parameters) == FALSE) {
			return (print(paste0("Error: parameters must be a vector.")))
		}
		if (is.character(parameters[[1]]) == FALSE) {
			return (print(paste0("Error: parameters[[1]] must be a string.")))
		}
	}
	if (is.null(headers) == FALSE) {
	}
	if (is.null(content) == FALSE) {
		if (is.character(content) == FALSE) {
			return (print(paste0("Error: content must be a string.")))
		}
	}
	if (is.null(parentIds) == FALSE) {
		if (is.vector(parentIds) == FALSE) {
			return (print(paste0("Error: parentIds must be a vector.")))
		}
		if (is.character(parentIds[[1]]) == FALSE) {
			return (print(paste0("Error: parentIds[[1]] must be a string.")))
		}
	}
	value <- list(
		Method = method,
		Resource = resource,
		RequestTemplate = requestTemplate,
		Parameters = parameters,
		Headers = headers,
		Content = content,
		ParentIds = parentIds)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIRequest"
	return(valueCleaned)
}
