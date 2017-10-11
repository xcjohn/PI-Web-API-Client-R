PISecurityRights <- function(ownerWebId = NULL, securityItem = NULL, userIdentity = NULL, links = NULL) {
	if (is.null(ownerWebId) == FALSE) {
		if (is.character(ownerWebId) == FALSE) {
			return (print(paste0("Error: ownerWebId must be a string.")))
		}
	}
	if (is.null(securityItem) == FALSE) {
		if (is.character(securityItem) == FALSE) {
			return (print(paste0("Error: securityItem must be a string.")))
		}
	}
	if (is.null(userIdentity) == FALSE) {
		if (is.character(userIdentity) == FALSE) {
			return (print(paste0("Error: userIdentity must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
	}
	value <- list(
	OwnerWebId = ownerWebId,
	SecurityItem = securityItem,
	UserIdentity = userIdentity,
	Links = links)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISecurityRights"
	return(valueCleaned)
}
