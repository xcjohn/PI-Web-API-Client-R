PIPoint <- function(webId = NULL, id = NULL, name = NULL, path = NULL, descriptor = NULL, pointClass = NULL, pointType = NULL, digitalSetName = NULL, span = NULL, zero = NULL, engineeringUnits = NULL, step = NULL, future = NULL, displayDigits = NULL, links = NULL, webException = NULL) {
	if (is.null(webId) == FALSE) {
		if (is.character(webId) == FALSE) {
			return (print(paste0("Error: webId must be a string.")))
		}
	}
	if (is.null(id) == FALSE) {
		if (check.integer(id) == FALSE) {
			return (print(paste0("Error: id must be an integer.")))
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
	if (is.null(descriptor) == FALSE) {
		if (is.character(descriptor) == FALSE) {
			return (print(paste0("Error: descriptor must be a string.")))
		}
	}
	if (is.null(pointClass) == FALSE) {
		if (is.character(pointClass) == FALSE) {
			return (print(paste0("Error: pointClass must be a string.")))
		}
	}
	if (is.null(pointType) == FALSE) {
		if (is.character(pointType) == FALSE) {
			return (print(paste0("Error: pointType must be a string.")))
		}
	}
	if (is.null(digitalSetName) == FALSE) {
		if (is.character(digitalSetName) == FALSE) {
			return (print(paste0("Error: digitalSetName must be a string.")))
		}
	}
	if (is.null(span) == FALSE) {
	}
	if (is.null(zero) == FALSE) {
	}
	if (is.null(engineeringUnits) == FALSE) {
		if (is.character(engineeringUnits) == FALSE) {
			return (print(paste0("Error: engineeringUnits must be a string.")))
		}
	}
	if (is.null(step) == FALSE) {
		if (is.logical(step) == FALSE) {
			return (print(paste0("Error: step must be a boolean.")))
		}
	}
	if (is.null(future) == FALSE) {
		if (is.logical(future) == FALSE) {
			return (print(paste0("Error: future must be a boolean.")))
		}
	}
	if (is.null(displayDigits) == FALSE) {
		if (check.integer(displayDigits) == FALSE) {
			return (print(paste0("Error: displayDigits must be an integer.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIPointLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIPointLinks.")))
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
		Path = path,
		Descriptor = descriptor,
		PointClass = pointClass,
		PointType = pointType,
		DigitalSetName = digitalSetName,
		Span = span,
		Zero = zero,
		EngineeringUnits = engineeringUnits,
		Step = step,
		Future = future,
		DisplayDigits = displayDigits,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIPoint"
	return(valueCleaned)
}
