PIItemsItemElement <- function(items = NULL, links = NULL) {
	if (is.null(items) == FALSE) {
		if (is.vector(items) == FALSE) {
			return (print(paste0("Error: items must be a vector.")))
		}
	}
	if (is.null(links) == FALSE) {
		className <- attr(links, "className")
		if ((is.null(className)) || (className != "PIPaginationLinks")) {
			return (print(paste0("Error: the class from the parameter links should be PIPaginationLinks.")))
		}
	}
	value <- list(
		Items = items,
		Links = links)
	valueCleaned <- rmNullObs(value)
	attr(valueCleaned, "className") <- "PIItemsItemElement"
	return(valueCleaned)
}
