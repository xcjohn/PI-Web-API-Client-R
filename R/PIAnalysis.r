PIAnalysis <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, analysisRulePlugInName = NULL, autoCreated = NULL, categoryNames = NULL, groupId = NULL, hasNotification = NULL, hasTarget = NULL, hasTemplate = NULL, isConfigured = NULL, isTimeRuleDefinedByTemplate = NULL, maximumQueueSize = NULL, outputTime = NULL, priority = NULL, publishResults = NULL, status = NULL, targetWebId = NULL, templateName = NULL, timeRulePlugInName = NULL, links = NULL, webException = NULL) {
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
	if (is.null(analysisRulePlugInName) == FALSE) {
		if (is.character(analysisRulePlugInName) == FALSE) {
			return (print(paste0("Error: analysisRulePlugInName must be a string.")))
		}
	}
	if (is.null(autoCreated) == FALSE) {
		if (is.logical(autoCreated) == FALSE) {
			return (print(paste0("Error: autoCreated must be a boolean.")))
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
	if (is.null(groupId) == FALSE) {
		if (check.integer(groupId) == FALSE) {
			return (print(paste0("Error: groupId must be an integer.")))
		}
	}
	if (is.null(hasNotification) == FALSE) {
		if (is.logical(hasNotification) == FALSE) {
			return (print(paste0("Error: hasNotification must be a boolean.")))
		}
	}
	if (is.null(hasTarget) == FALSE) {
		if (is.logical(hasTarget) == FALSE) {
			return (print(paste0("Error: hasTarget must be a boolean.")))
		}
	}
	if (is.null(hasTemplate) == FALSE) {
		if (is.logical(hasTemplate) == FALSE) {
			return (print(paste0("Error: hasTemplate must be a boolean.")))
		}
	}
	if (is.null(isConfigured) == FALSE) {
		if (is.logical(isConfigured) == FALSE) {
			return (print(paste0("Error: isConfigured must be a boolean.")))
		}
	}
	if (is.null(isTimeRuleDefinedByTemplate) == FALSE) {
		if (is.logical(isTimeRuleDefinedByTemplate) == FALSE) {
			return (print(paste0("Error: isTimeRuleDefinedByTemplate must be a boolean.")))
		}
	}
	if (is.null(maximumQueueSize) == FALSE) {
		if (check.integer(maximumQueueSize) == FALSE) {
			return (print(paste0("Error: maximumQueueSize must be an integer.")))
		}
	}
	if (is.null(outputTime) == FALSE) {
		if (is.character(outputTime) == FALSE) {
			return (print(paste0("Error: outputTime must be a string.")))
		}
	}
	if (is.null(priority) == FALSE) {
		if (is.character(priority) == FALSE) {
			return (print(paste0("Error: priority must be a string.")))
		}
	}
	if (is.null(publishResults) == FALSE) {
		if (is.logical(publishResults) == FALSE) {
			return (print(paste0("Error: publishResults must be a boolean.")))
		}
	}
	if (is.null(status) == FALSE) {
		if (is.character(status) == FALSE) {
			return (print(paste0("Error: status must be a string.")))
		}
	}
	if (is.null(targetWebId) == FALSE) {
		if (is.character(targetWebId) == FALSE) {
			return (print(paste0("Error: targetWebId must be a string.")))
		}
	}
	if (is.null(templateName) == FALSE) {
		if (is.character(templateName) == FALSE) {
			return (print(paste0("Error: templateName must be a string.")))
		}
	}
	if (is.null(timeRulePlugInName) == FALSE) {
		if (is.character(timeRulePlugInName) == FALSE) {
			return (print(paste0("Error: timeRulePlugInName must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIAnalysisLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIAnalysisLinks.")))
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
		AnalysisRulePlugInName = analysisRulePlugInName,
		AutoCreated = autoCreated,
		CategoryNames = categoryNames,
		GroupId = groupId,
		HasNotification = hasNotification,
		HasTarget = hasTarget,
		HasTemplate = hasTemplate,
		IsConfigured = isConfigured,
		IsTimeRuleDefinedByTemplate = isTimeRuleDefinedByTemplate,
		MaximumQueueSize = maximumQueueSize,
		OutputTime = outputTime,
		Priority = priority,
		PublishResults = publishResults,
		Status = status,
		TargetWebId = targetWebId,
		TemplateName = templateName,
		TimeRulePlugInName = timeRulePlugInName,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAnalysis"
	return(valueCleaned)
}
