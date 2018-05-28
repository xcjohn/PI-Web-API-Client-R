PISecurityMappingLinks <- function(self = NULL, assetServer = NULL, securityIdentity = NULL, security = NULL, securityEntries = NULL) {
	if (is.null(self) == FALSE) {
		if (is.character(self) == FALSE) {
			return (print(paste0("Error: self must be a string.")))
		}
	}
	if (is.null(assetServer) == FALSE) {
		if (is.character(assetServer) == FALSE) {
			return (print(paste0("Error: assetServer must be a string.")))
		}
	}
	if (is.null(securityIdentity) == FALSE) {
		if (is.character(securityIdentity) == FALSE) {
			return (print(paste0("Error: securityIdentity must be a string.")))
		}
	}
	if (is.null(security) == FALSE) {
		if (is.character(security) == FALSE) {
			return (print(paste0("Error: security must be a string.")))
		}
	}
	if (is.null(securityEntries) == FALSE) {
		if (is.character(securityEntries) == FALSE) {
			return (print(paste0("Error: securityEntries must be a string.")))
		}
	}
	value <- list(
		Self = self,
		AssetServer = assetServer,
		SecurityIdentity = securityIdentity,
		Security = security,
		SecurityEntries = securityEntries)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISecurityMappingLinks"
	return(valueCleaned)
}
