PIAnalysisTemplate <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, analysisRulePlugInName = NULL, categoryNames = NULL, createEnabled = NULL, groupId = NULL, hasNotificationTemplate = NULL, hasTarget = NULL, outputTime = NULL, targetName = NULL, timeRulePlugInName = NULL, links = NULL, webException = NULL) {
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
	if (is.null(categoryNames) == FALSE) {
		if (is.vector(categoryNames) == FALSE) {
			return (print(paste0("Error: categoryNames must be a vector.")))
		}
		if (is.character(categoryNames[[1]]) == FALSE) {
			return (print(paste0("Error: categoryNames[[1]] must be a string.")))
		}
	}
	if (is.null(createEnabled) == FALSE) {
		if (is.logical(createEnabled) == FALSE) {
			return (print(paste0("Error: createEnabled must be a boolean.")))
		}
	}
	if (is.null(groupId) == FALSE) {
		if (check.integer(groupId) == FALSE) {
			return (print(paste0("Error: groupId must be an integer.")))
		}
	}
	if (is.null(hasNotificationTemplate) == FALSE) {
		if (is.logical(hasNotificationTemplate) == FALSE) {
			return (print(paste0("Error: hasNotificationTemplate must be a boolean.")))
		}
	}
	if (is.null(hasTarget) == FALSE) {
		if (is.logical(hasTarget) == FALSE) {
			return (print(paste0("Error: hasTarget must be a boolean.")))
		}
	}
	if (is.null(outputTime) == FALSE) {
		if (is.character(outputTime) == FALSE) {
			return (print(paste0("Error: outputTime must be a string.")))
		}
	}
	if (is.null(targetName) == FALSE) {
		if (is.character(targetName) == FALSE) {
			return (print(paste0("Error: targetName must be a string.")))
		}
	}
	if (is.null(timeRulePlugInName) == FALSE) {
		if (is.character(timeRulePlugInName) == FALSE) {
			return (print(paste0("Error: timeRulePlugInName must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIAnalysisTemplateLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIAnalysisTemplateLinks.")))
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
		CategoryNames = categoryNames,
		CreateEnabled = createEnabled,
		GroupId = groupId,
		HasNotificationTemplate = hasNotificationTemplate,
		HasTarget = hasTarget,
		OutputTime = outputTime,
		TargetName = targetName,
		TimeRulePlugInName = timeRulePlugInName,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIAnalysisTemplate"
	return(valueCleaned)
}
