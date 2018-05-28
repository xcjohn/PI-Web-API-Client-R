PIEventFrame <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, templateName = NULL, hasChildren = NULL, categoryNames = NULL, extendedProperties = NULL, startTime = NULL, endTime = NULL, severity = NULL, acknowledgedBy = NULL, acknowledgedDate = NULL, canBeAcknowledged = NULL, isAcknowledged = NULL, isAnnotated = NULL, isLocked = NULL, areValuesCaptured = NULL, refElementWebIds = NULL, security = NULL, links = NULL, webException = NULL) {
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
	if (is.null(templateName) == FALSE) {
		if (is.character(templateName) == FALSE) {
			return (print(paste0("Error: templateName must be a string.")))
		}
	}
	if (is.null(hasChildren) == FALSE) {
		if (is.logical(hasChildren) == FALSE) {
			return (print(paste0("Error: hasChildren must be a boolean.")))
		}
	}
	if (is.null(categoryNames) == FALSE) {
		if (is.vector(categoryNames) == FALSE) {
			return (print(paste0("Error: categoryNames must be a vector.")))
		}
		if (is.character(categoryNames[[1]]) == FALSE) {
			return (print(paste0("Error: categoryNames[[1]] must be a string.")))
		}
	}
	if (is.null(extendedProperties) == FALSE) {
	}
	if (is.null(startTime) == FALSE) {
		if (is.character(startTime) == FALSE) {
			return (print(paste0("Error: startTime must be a string.")))
		}
	}
	if (is.null(endTime) == FALSE) {
		if (is.character(endTime) == FALSE) {
			return (print(paste0("Error: endTime must be a string.")))
		}
	}
	if (is.null(severity) == FALSE) {
		if (is.character(severity) == FALSE) {
			return (print(paste0("Error: severity must be a string.")))
		}
	}
	if (is.null(acknowledgedBy) == FALSE) {
		if (is.character(acknowledgedBy) == FALSE) {
			return (print(paste0("Error: acknowledgedBy must be a string.")))
		}
	}
	if (is.null(acknowledgedDate) == FALSE) {
		if (is.character(acknowledgedDate) == FALSE) {
			return (print(paste0("Error: acknowledgedDate must be a string.")))
		}
	}
	if (is.null(canBeAcknowledged) == FALSE) {
		if (is.logical(canBeAcknowledged) == FALSE) {
			return (print(paste0("Error: canBeAcknowledged must be a boolean.")))
		}
	}
	if (is.null(isAcknowledged) == FALSE) {
		if (is.logical(isAcknowledged) == FALSE) {
			return (print(paste0("Error: isAcknowledged must be a boolean.")))
		}
	}
	if (is.null(isAnnotated) == FALSE) {
		if (is.logical(isAnnotated) == FALSE) {
			return (print(paste0("Error: isAnnotated must be a boolean.")))
		}
	}
	if (is.null(isLocked) == FALSE) {
		if (is.logical(isLocked) == FALSE) {
			return (print(paste0("Error: isLocked must be a boolean.")))
		}
	}
	if (is.null(areValuesCaptured) == FALSE) {
		if (is.logical(areValuesCaptured) == FALSE) {
			return (print(paste0("Error: areValuesCaptured must be a boolean.")))
		}
	}
	if (is.null(refElementWebIds) == FALSE) {
		if (is.vector(refElementWebIds) == FALSE) {
			return (print(paste0("Error: refElementWebIds must be a vector.")))
		}
		if (is.character(refElementWebIds[[1]]) == FALSE) {
			return (print(paste0("Error: refElementWebIds[[1]] must be a string.")))
		}
	}
	if (is.null(security) == FALSE) {
		className <- attr(security, "className")
		if ((is.null(className)) || (className != "PISecurity")) {
			return (print(paste0("Error: the class from the parameter security should be PISecurity.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIEventFrameLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIEventFrameLinks.")))
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
		TemplateName = templateName,
		HasChildren = hasChildren,
		CategoryNames = categoryNames,
		ExtendedProperties = extendedProperties,
		StartTime = startTime,
		EndTime = endTime,
		Severity = severity,
		AcknowledgedBy = acknowledgedBy,
		AcknowledgedDate = acknowledgedDate,
		CanBeAcknowledged = canBeAcknowledged,
		IsAcknowledged = isAcknowledged,
		IsAnnotated = isAnnotated,
		IsLocked = isLocked,
		AreValuesCaptured = areValuesCaptured,
		RefElementWebIds = refElementWebIds,
		Security = security,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIEventFrame"
	return(valueCleaned)
}
