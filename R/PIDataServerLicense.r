PIDataServerLicense <- function(amountLeft = NULL, amountUsed = NULL, name = NULL, totalAmount = NULL, links = NULL, webException = NULL) {
	if (is.null(amountLeft) == FALSE) {
		if (is.character(amountLeft) == FALSE) {
			return (print(paste0("Error: amountLeft must be a string.")))
		}
	}
	if (is.null(amountUsed) == FALSE) {
		if (is.character(amountUsed) == FALSE) {
			return (print(paste0("Error: amountUsed must be a string.")))
		}
	}
	if (is.null(name) == FALSE) {
		if (is.character(name) == FALSE) {
			return (print(paste0("Error: name must be a string.")))
		}
	}
	if (is.null(totalAmount) == FALSE) {
		if (is.character(totalAmount) == FALSE) {
			return (print(paste0("Error: totalAmount must be a string.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIDataServerLicenseLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIDataServerLicenseLinks.")))
		}
	}
	if (is.null(webException) == FALSE) {
		className <- attr(webException, "className")
		if ((is.null(className)) || (className != "PIWebException")) {
			return (print(paste0("Error: the class from the parameter webException should be PIWebException.")))
		}
	}
	value <- list(
		AmountLeft = amountLeft,
		AmountUsed = amountUsed,
		Name = name,
		TotalAmount = totalAmount,
		Links = links,
		WebException = webException)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIDataServerLicense"
	return(valueCleaned)
}
