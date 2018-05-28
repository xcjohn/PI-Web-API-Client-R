PINotificationRule <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, autoCreated = NULL, categoryNames = NULL, criteria = NULL, multiTriggerEventOption = NULL, nonrepetitionInterval = NULL, resendInterval = NULL, status = NULL, templateName = NULL, webException = NULL) {
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
	if (is.null(criteria) == FALSE) {
		if (is.character(criteria) == FALSE) {
			return (print(paste0("Error: criteria must be a string.")))
		}
	}
	if (is.null(multiTriggerEventOption) == FALSE) {
		if (is.character(multiTriggerEventOption) == FALSE) {
			return (print(paste0("Error: multiTriggerEventOption must be a string.")))
		}
	}
	if (is.null(nonrepetitionInterval) == FALSE) {
		if (is.character(nonrepetitionInterval) == FALSE) {
			return (print(paste0("Error: nonrepetitionInterval must be a string.")))
		}
	}
	if (is.null(resendInterval) == FALSE) {
		if (is.character(resendInterval) == FALSE) {
			return (print(paste0("Error: resendInterval must be a string.")))
		}
	}
	if (is.null(status) == FALSE) {
		if (is.character(status) == FALSE) {
			return (print(paste0("Error: status must be a string.")))
		}
	}
	if (is.null(templateName) == FALSE) {
		if (is.character(templateName) == FALSE) {
			return (print(paste0("Error: templateName must be a string.")))
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
		AutoCreated = autoCreated,
		CategoryNames = categoryNames,
		Criteria = criteria,
		MultiTriggerEventOption = multiTriggerEventOption,
		NonrepetitionInterval = nonrepetitionInterval,
		ResendInterval = resendInterval,
		Status = status,
		TemplateName = templateName,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PINotificationRule"
	return(valueCleaned)
}
