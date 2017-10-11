PISystemLanding <- function(productTitle = NULL, productVersion = NULL, links = NULL) {
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
	}
	value <- list(
	ProductTitle = productTitle,
	ProductVersion = productVersion,
	Links = links)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PISystemLanding"
	return(valueCleaned)
}
