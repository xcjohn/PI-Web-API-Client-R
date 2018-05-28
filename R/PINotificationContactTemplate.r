PINotificationContactTemplate <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, available = NULL, configString = NULL, contactType = NULL, plugInName = NULL, hasChildren = NULL, maximumRetries = NULL, minimumAcknowledgements = NULL, notifyWhenInstanceEnded = NULL, escalationTimeout = NULL, retryInterval = NULL, links = NULL, webException = NULL) {
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
	if (is.null(available) == FALSE) {
		if (is.logical(available) == FALSE) {
			return (print(paste0("Error: available must be a boolean.")))
		}
	}
	if (is.null(configString) == FALSE) {
		if (is.character(configString) == FALSE) {
			return (print(paste0("Error: configString must be a string.")))
		}
	}
	if (is.null(contactType) == FALSE) {
		if (is.character(contactType) == FALSE) {
			return (print(paste0("Error: contactType must be a string.")))
		}
	}
	if (is.null(plugInName) == FALSE) {
		if (is.character(plugInName) == FALSE) {
			return (print(paste0("Error: plugInName must be a string.")))
		}
	}
	if (is.null(hasChildren) == FALSE) {
		if (is.logical(hasChildren) == FALSE) {
			return (print(paste0("Error: hasChildren must be a boolean.")))
		}
	}
	if (is.null(maximumRetries) == FALSE) {
		if (check.integer(maximumRetries) == FALSE) {
			return (print(paste0("Error: maximumRetries must be an integer.")))
		}
	}
	if (is.null(minimumAcknowledgements) == FALSE) {
		if (check.integer(minimumAcknowledgements) == FALSE) {
			return (print(paste0("Error: minimumAcknowledgements must be an integer.")))
		}
	}
	if (is.null(notifyWhenInstanceEnded) == FALSE) {
		if (is.logical(notifyWhenInstanceEnded) == FALSE) {
			return (print(paste0("Error: notifyWhenInstanceEnded must be a boolean.")))
		}
	}
	if (is.null(escalationTimeout) == FALSE) {
		if (is.character(escalationTimeout) == FALSE) {
			return (print(paste0("Error: escalationTimeout must be a string.")))
		}
	}
	if (is.null(retryInterval) == FALSE) {
		if (is.character(retryInterval) == FALSE) {
			return (print(paste0("Error: retryInterval must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PINotificationContactTemplateLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PINotificationContactTemplateLinks.")))
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
		Available = available,
		ConfigString = configString,
		ContactType = contactType,
		PlugInName = plugInName,
		HasChildren = hasChildren,
		MaximumRetries = maximumRetries,
		MinimumAcknowledgements = minimumAcknowledgements,
		NotifyWhenInstanceEnded = notifyWhenInstanceEnded,
		EscalationTimeout = escalationTimeout,
		RetryInterval = retryInterval,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PINotificationContactTemplate"
	return(valueCleaned)
}
