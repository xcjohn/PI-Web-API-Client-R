PIChannelInstance <- function(id = NULL, startTime = NULL, lastMessageSentTime = NULL, sentMessageCount = NULL, webException = NULL) {
	if (is.null(id) == FALSE) {
		if (is.character(id) == FALSE) {
			return (print(paste0("Error: id must be a string.")))
		}
	}
	if (is.null(startTime) == FALSE) {
		if (is.character(startTime) == FALSE) {
			return (print(paste0("Error: startTime must be a string.")))
		}
	}
	if (is.null(lastMessageSentTime) == FALSE) {
		if (is.character(lastMessageSentTime) == FALSE) {
			return (print(paste0("Error: lastMessageSentTime must be a string.")))
		}
	}
	if (is.null(sentMessageCount) == FALSE) {
		if (check.integer(sentMessageCount) == FALSE) {
			return (print(paste0("Error: sentMessageCount must be an integer.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Id = id,
		StartTime = startTime,
		LastMessageSentTime = lastMessageSentTime,
		SentMessageCount = sentMessageCount,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIChannelInstance"
	return(valueCleaned)
}
