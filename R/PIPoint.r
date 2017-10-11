PIPoint <- function(webId = NULL, id = NULL, name = NULL, path = NULL, descriptor = NULL, pointClass = NULL, pointType = NULL, digitalSetName = NULL, engineeringUnits = NULL, step = NULL, future = NULL, links = NULL) {
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
	if (is.null(links) == FALSE) {
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
	EngineeringUnits = engineeringUnits,
	Step = step,
	Future = future,
	Links = links)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIPoint"
	return(valueCleaned)
}
