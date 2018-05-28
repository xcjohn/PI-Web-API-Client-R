PINotificationRuleSubscriber <- function(webId = NULL, id = NULL, name = NULL, description = NULL, path = NULL, configString = NULL, contactTemplateName = NULL, contactType = NULL, deliveryFormatName = NULL, plugInName = NULL, escalationTimeout = NULL, maximumRetries = NULL, notifyOption = NULL, retryInterval = NULL, webException = NULL) {
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
	if (is.null(configString) == FALSE) {
		if (is.character(configString) == FALSE) {
			return (print(paste0("Error: configString must be a string.")))
		}
	}
	if (is.null(contactTemplateName) == FALSE) {
		if (is.character(contactTemplateName) == FALSE) {
			return (print(paste0("Error: contactTemplateName must be a string.")))
		}
	}
	if (is.null(contactType) == FALSE) {
		if (is.character(contactType) == FALSE) {
			return (print(paste0("Error: contactType must be a string.")))
		}
	}
	if (is.null(deliveryFormatName) == FALSE) {
		if (is.character(deliveryFormatName) == FALSE) {
			return (print(paste0("Error: deliveryFormatName must be a string.")))
		}
	}
	if (is.null(plugInName) == FALSE) {
		if (is.character(plugInName) == FALSE) {
			return (print(paste0("Error: plugInName must be a string.")))
		}
	}
	if (is.null(escalationTimeout) == FALSE) {
		if (is.character(escalationTimeout) == FALSE) {
			return (print(paste0("Error: escalationTimeout must be a string.")))
		}
	}
	if (is.null(maximumRetries) == FALSE) {
		if (check.integer(maximumRetries) == FALSE) {
			return (print(paste0("Error: maximumRetries must be an integer.")))
		}
	}
	if (is.null(notifyOption) == FALSE) {
		if (is.character(notifyOption) == FALSE) {
			return (print(paste0("Error: notifyOption must be a string.")))
		}
	}
	if (is.null(retryInterval) == FALSE) {
		if (is.character(retryInterval) == FALSE) {
			return (print(paste0("Error: retryInterval must be a string.")))
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
		ConfigString = configString,
		ContactTemplateName = contactTemplateName,
		ContactType = contactType,
		DeliveryFormatName = deliveryFormatName,
		PlugInName = plugInName,
		EscalationTimeout = escalationTimeout,
		MaximumRetries = maximumRetries,
		NotifyOption = notifyOption,
		RetryInterval = retryInterval,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PINotificationRuleSubscriber"
	return(valueCleaned)
}
