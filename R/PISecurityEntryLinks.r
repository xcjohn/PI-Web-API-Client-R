PISecurityEntryLinks <- function(self = NULL, securableObject = NULL, securityIdentity = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(securableObject) == FALSE) {
		if (is.character(securableObject) == FALSE) {
			return (print(paste0("Error: securableObject must be a string.")))
		}
	}
	if (is.null(securityIdentity) == FALSE) {
		if (is.character(securityIdentity) == FALSE) {
			return (print(paste0("Error: securityIdentity must be a string.")))
		}
	}
	value <- list(
		Self = self,
		SecurableObject = securableObject,
		SecurityIdentity = securityIdentity)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISecurityEntryLinks"
	return(valueCleaned)
}
