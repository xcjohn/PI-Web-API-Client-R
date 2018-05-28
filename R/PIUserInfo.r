PIUserInfo <- function(identityType = NULL, name = NULL, isAuthenticated = NULL, sID = NULL, impersonationLevel = NULL, webException = NULL) {
	if (is.null(identityType) == FALSE) {
		if (is.character(identityType) == FALSE) {
			return (print(paste0("Error: identityType must be a string.")))
		}
	}
	if (is.null(name) == FALSE) {
		if (is.character(name) == FALSE) {
			return (print(paste0("Error: name must be a string.")))
		}
	}
	if (is.null(isAuthenticated) == FALSE) {
		if (is.logical(isAuthenticated) == FALSE) {
			return (print(paste0("Error: isAuthenticated must be a boolean.")))
		}
	}
	if (is.null(sID) == FALSE) {
		if (is.character(sID) == FALSE) {
			return (print(paste0("Error: sID must be a string.")))
		}
	}
	if (is.null(impersonationLevel) == FALSE) {
		if (is.character(impersonationLevel) == FALSE) {
			return (print(paste0("Error: impersonationLevel must be a string.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		IdentityType = identityType,
		Name = name,
		IsAuthenticated = isAuthenticated,
		SID = sID,
		ImpersonationLevel = impersonationLevel,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIUserInfo"
	return(valueCleaned)
}
