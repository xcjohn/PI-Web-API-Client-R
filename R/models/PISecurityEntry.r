PISecurityEntry <- function(name = NULL, securityIdentityName = NULL, allowRights = NULL, denyRights = NULL, links = NULL) {
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
	}
	value <- list(
	Name = name,
	SecurityIdentityName = securityIdentityName,
	AllowRights = allowRights,
	DenyRights = denyRights,
	Links = links)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISecurityEntry"
	return(valueCleaned)
}
