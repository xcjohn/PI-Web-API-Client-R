PIUserInfo <- function(identityType = NULL, name = NULL, isAuthenticated = NULL, sID = NULL, impersonationLevel = NULL) {
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
	value <- list(
	IdentityType = identityType,
	Name = name,
	IsAuthenticated = isAuthenticated,
	SID = sID,
	ImpersonationLevel = impersonationLevel)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIUserInfo"
	return(valueCleaned)
}
