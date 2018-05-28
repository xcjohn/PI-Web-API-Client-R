PISecurityEntry <- function(name = NULL, securityIdentityName = NULL, allowRights = NULL, denyRights = NULL, links = NULL, webException = NULL) {
	if (is.null(name) == FALSE) {
		if (is.character(name) == FALSE) {
			return (print(paste0("Error: name must be a string.")))
		}
	}
	if (is.null(securityIdentityName) == FALSE) {
		if (is.character(securityIdentityName) == FALSE) {
			return (print(paste0("Error: securityIdentityName must be a string.")))
		}
	}
	if (is.null(allowRights) == FALSE) {
		if (is.vector(allowRights) == FALSE) {
			return (print(paste0("Error: allowRights must be a vector.")))
		}
		if (is.character(allowRights[[1]]) == FALSE) {
			return (print(paste0("Error: allowRights[[1]] must be a string.")))
		}
	}
	if (is.null(denyRights) == FALSE) {
		if (is.vector(denyRights) == FALSE) {
			return (print(paste0("Error: denyRights must be a vector.")))
		}
		if (is.character(denyRights[[1]]) == FALSE) {
			return (print(paste0("Error: denyRights[[1]] must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PISecurityEntryLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PISecurityEntryLinks.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		Name = name,
		SecurityIdentityName = securityIdentityName,
		AllowRights = allowRights,
		DenyRights = denyRights,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISecurityEntry"
	return(valueCleaned)
}
