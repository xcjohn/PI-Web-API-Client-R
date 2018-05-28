PISystemLanding <- function(productTitle = NULL, productVersion = NULL, links = NULL, webException = NULL) {
	if (is.null(productTitle) == FALSE) {
		if (is.character(productTitle) == FALSE) {
			return (print(paste0("Error: productTitle must be a string.")))
		}
	}
	if (is.null(productVersion) == FALSE) {
		if (is.character(productVersion) == FALSE) {
			return (print(paste0("Error: productVersion must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PISystemLandingLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PISystemLandingLinks.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		ProductTitle = productTitle,
		ProductVersion = productVersion,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISystemLanding"
	return(valueCleaned)
}
