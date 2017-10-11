elementApi <- R6Class("elementApi",
	private = list(),
	public = list(
		serviceBase = NULL,
		authType = NULL,
		username = NULL,
		password = NULL,
		validateSSL = NULL,
		debug = NULL,
		initialize = function(baseUrl, authType, username, password, validateSSL, debug) {
			self$serviceBase <- baseUrl
			self$username <- username
			self$password <- password
			self$authType <- authType
			self$validateSSL <- validateSSL
			self$debug <- debug
		},
		getByPath = function(path, selectedFields) {
			queryParameters <- list()
			if (is.null(path) || path == "") {
				return (paste0("Error: required parameter path was null or undefined"))
			}
			if (is.character(path) == FALSE) {
				return (print(paste0("Error: path must be a string.")))
			}
			queryParameters$path <- path
			localVarPath <- paste(c(self$serviceBase, '/elements'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIElement"
			}
			return (contentResponse)
		},
		get = function(webId, selectedFields) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIElement"
			}
			return (contentResponse)
		},
		update = function(webId, PIElement) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIElement) || PIElement == "") {
				return (paste0("Error: required parameter PIElement was null or undefined"))
			}
			className <- attr(PIElement, "className")
			if ((is.null(className)) || (className != "PIElement")) {
				return (print(paste0("Error: the class from the parameter PIElement should be PIElement.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId), collapse = "")
			res <- patchHttpRequest(localVarPath, PIElement, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		delete = function(webId) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId), collapse = "")
			res <- deleteHttpRequest(localVarPath, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getAnalyses = function(webId, maxCount, selectedFields, sortField, sortOrder, startIndex) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/analyses'), collapse = "")
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				queryParameters$sortField <- sortField
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				queryParameters$startIndex <- startIndex
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAnalysis"
			}
			return (contentResponse)
		},
		createAnalysis = function(webId, PIAnalysis) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAnalysis) || PIAnalysis == "") {
				return (paste0("Error: required parameter PIAnalysis was null or undefined"))
			}
			className <- attr(PIAnalysis, "className")
			if ((is.null(className)) || (className != "PIAnalysis")) {
				return (print(paste0("Error: the class from the parameter PIAnalysis should be PIAnalysis.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/analyses'), collapse = "")
			res <- postHttpRequest(localVarPath, PIAnalysis, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getAttributes = function(webId, categoryName, maxCount, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortField, sortOrder, startIndex, templateName, valueType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/attributes'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				queryParameters$searchFullHierarchy <- searchFullHierarchy
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				queryParameters$showExcluded <- showExcluded
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				queryParameters$showHidden <- showHidden
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				queryParameters$sortField <- sortField
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				queryParameters$startIndex <- startIndex
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(valueType) == FALSE && is.null(valueType) == FALSE && valueType != "") {
				queryParameters$valueType <- valueType
				if (is.character(valueType) == FALSE) {
					return (print(paste0("Error: valueType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAttribute"
			}
			return (contentResponse)
		},
		createAttribute = function(webId, PIAttribute) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAttribute) || PIAttribute == "") {
				return (paste0("Error: required parameter PIAttribute was null or undefined"))
			}
			className <- attr(PIAttribute, "className")
			if ((is.null(className)) || (className != "PIAttribute")) {
				return (print(paste0("Error: the class from the parameter PIAttribute should be PIAttribute.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/attributes'), collapse = "")
			res <- postHttpRequest(localVarPath, PIAttribute, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getCategories = function(webId, selectedFields) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/categories'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsElementCategory"
			}
			return (contentResponse)
		},
		createConfig = function(webId, includeChildElements) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/config'), collapse = "")
			if (missing(includeChildElements) == FALSE && is.null(includeChildElements) == FALSE && includeChildElements != "") {
				queryParameters$includeChildElements <- includeChildElements
				if (is.logical(includeChildElements) == FALSE) {
					return (print(paste0("Error: includeChildElements must be a boolean.")))
				}
			}
			res <- postHttpRequest(localVarPath, , self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		findElementAttributes = function(webId, attributeCategory, attributeDescriptionFilter, attributeNameFilter, attributeType, elementCategory, elementDescriptionFilter, elementNameFilter, elementTemplate, elementType, maxCount, searchFullHierarchy, selectedFields, sortField, sortOrder, startIndex) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/elementattributes'), collapse = "")
			if (missing(attributeCategory) == FALSE && is.null(attributeCategory) == FALSE && attributeCategory != "") {
				queryParameters$attributeCategory <- attributeCategory
				if (is.character(attributeCategory) == FALSE) {
					return (print(paste0("Error: attributeCategory must be a string.")))
				}
			}
			if (missing(attributeDescriptionFilter) == FALSE && is.null(attributeDescriptionFilter) == FALSE && attributeDescriptionFilter != "") {
				queryParameters$attributeDescriptionFilter <- attributeDescriptionFilter
				if (is.character(attributeDescriptionFilter) == FALSE) {
					return (print(paste0("Error: attributeDescriptionFilter must be a string.")))
				}
			}
			if (missing(attributeNameFilter) == FALSE && is.null(attributeNameFilter) == FALSE && attributeNameFilter != "") {
				queryParameters$attributeNameFilter <- attributeNameFilter
				if (is.character(attributeNameFilter) == FALSE) {
					return (print(paste0("Error: attributeNameFilter must be a string.")))
				}
			}
			if (missing(attributeType) == FALSE && is.null(attributeType) == FALSE && attributeType != "") {
				queryParameters$attributeType <- attributeType
				if (is.character(attributeType) == FALSE) {
					return (print(paste0("Error: attributeType must be a string.")))
				}
			}
			if (missing(elementCategory) == FALSE && is.null(elementCategory) == FALSE && elementCategory != "") {
				queryParameters$elementCategory <- elementCategory
				if (is.character(elementCategory) == FALSE) {
					return (print(paste0("Error: elementCategory must be a string.")))
				}
			}
			if (missing(elementDescriptionFilter) == FALSE && is.null(elementDescriptionFilter) == FALSE && elementDescriptionFilter != "") {
				queryParameters$elementDescriptionFilter <- elementDescriptionFilter
				if (is.character(elementDescriptionFilter) == FALSE) {
					return (print(paste0("Error: elementDescriptionFilter must be a string.")))
				}
			}
			if (missing(elementNameFilter) == FALSE && is.null(elementNameFilter) == FALSE && elementNameFilter != "") {
				queryParameters$elementNameFilter <- elementNameFilter
				if (is.character(elementNameFilter) == FALSE) {
					return (print(paste0("Error: elementNameFilter must be a string.")))
				}
			}
			if (missing(elementTemplate) == FALSE && is.null(elementTemplate) == FALSE && elementTemplate != "") {
				queryParameters$elementTemplate <- elementTemplate
				if (is.character(elementTemplate) == FALSE) {
					return (print(paste0("Error: elementTemplate must be a string.")))
				}
			}
			if (missing(elementType) == FALSE && is.null(elementType) == FALSE && elementType != "") {
				queryParameters$elementType <- elementType
				if (is.character(elementType) == FALSE) {
					return (print(paste0("Error: elementType must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				queryParameters$searchFullHierarchy <- searchFullHierarchy
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				queryParameters$sortField <- sortField
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				queryParameters$startIndex <- startIndex
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAttribute"
			}
			return (contentResponse)
		},
		getElements = function(webId, categoryName, descriptionFilter, elementType, maxCount, nameFilter, searchFullHierarchy, selectedFields, sortField, sortOrder, startIndex, templateName) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/elements'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(descriptionFilter) == FALSE && is.null(descriptionFilter) == FALSE && descriptionFilter != "") {
				queryParameters$descriptionFilter <- descriptionFilter
				if (is.character(descriptionFilter) == FALSE) {
					return (print(paste0("Error: descriptionFilter must be a string.")))
				}
			}
			if (missing(elementType) == FALSE && is.null(elementType) == FALSE && elementType != "") {
				queryParameters$elementType <- elementType
				if (is.character(elementType) == FALSE) {
					return (print(paste0("Error: elementType must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				queryParameters$searchFullHierarchy <- searchFullHierarchy
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				queryParameters$sortField <- sortField
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				queryParameters$startIndex <- startIndex
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsElement"
			}
			return (contentResponse)
		},
		createElement = function(webId, PIElement) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIElement) || PIElement == "") {
				return (paste0("Error: required parameter PIElement was null or undefined"))
			}
			className <- attr(PIElement, "className")
			if ((is.null(className)) || (className != "PIElement")) {
				return (print(paste0("Error: the class from the parameter PIElement should be PIElement.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/elements'), collapse = "")
			res <- postHttpRequest(localVarPath, PIElement, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getEventFrames = function(webId, canBeAcknowledged, categoryName, endTime, isAcknowledged, maxCount, nameFilter, searchMode, selectedFields, severity, sortField, sortOrder, startIndex, startTime, templateName) {
			queryParameters <- generateListForQueryString(severity, "severity")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/eventframes'), collapse = "")
			if (missing(canBeAcknowledged) == FALSE && is.null(canBeAcknowledged) == FALSE && canBeAcknowledged != "") {
				queryParameters$canBeAcknowledged <- canBeAcknowledged
				if (is.logical(canBeAcknowledged) == FALSE) {
					return (print(paste0("Error: canBeAcknowledged must be a boolean.")))
				}
			}
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				queryParameters$endTime <- endTime
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(isAcknowledged) == FALSE && is.null(isAcknowledged) == FALSE && isAcknowledged != "") {
				queryParameters$isAcknowledged <- isAcknowledged
				if (is.logical(isAcknowledged) == FALSE) {
					return (print(paste0("Error: isAcknowledged must be a boolean.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchMode) == FALSE && is.null(searchMode) == FALSE && searchMode != "") {
				queryParameters$searchMode <- searchMode
				if (is.character(searchMode) == FALSE) {
					return (print(paste0("Error: searchMode must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(severity) == FALSE && is.null(severity) == FALSE && severity != "") {
				queryParameters$severity <- severity
				if (is.vector(severity) == FALSE) {
					return (print(paste0("Error: severity must be a vector.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				queryParameters$sortField <- sortField
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				queryParameters$startIndex <- startIndex
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				queryParameters$startTime <- startTime
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsEventFrame"
			}
			return (contentResponse)
		},
		getReferencedElements = function(webId, categoryName, descriptionFilter, elementType, maxCount, nameFilter, selectedFields, sortField, sortOrder, startIndex, templateName) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/referencedelements'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(descriptionFilter) == FALSE && is.null(descriptionFilter) == FALSE && descriptionFilter != "") {
				queryParameters$descriptionFilter <- descriptionFilter
				if (is.character(descriptionFilter) == FALSE) {
					return (print(paste0("Error: descriptionFilter must be a string.")))
				}
			}
			if (missing(elementType) == FALSE && is.null(elementType) == FALSE && elementType != "") {
				queryParameters$elementType <- elementType
				if (is.character(elementType) == FALSE) {
					return (print(paste0("Error: elementType must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				queryParameters$sortField <- sortField
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				queryParameters$startIndex <- startIndex
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsElement"
			}
			return (contentResponse)
		},
		addReferencedElement = function(webId, referencedElementWebId, referenceType) {
			queryParameters <- generateListForQueryString(referencedElementWebId, "referencedElementWebId")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(referencedElementWebId) || referencedElementWebId == "") {
				return (paste0("Error: required parameter referencedElementWebId was null or undefined"))
			}
			if (is.vector(referencedElementWebId) == FALSE) {
				return (print(paste0("Error: referencedElementWebId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/referencedelements'), collapse = "")
			if (missing(referenceType) == FALSE && is.null(referenceType) == FALSE && referenceType != "") {
				queryParameters$referenceType <- referenceType
				if (is.character(referenceType) == FALSE) {
					return (print(paste0("Error: referenceType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, , self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		removeReferencedElement = function(webId, referencedElementWebId) {
			queryParameters <- generateListForQueryString(referencedElementWebId, "referencedElementWebId")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(referencedElementWebId) || referencedElementWebId == "") {
				return (paste0("Error: required parameter referencedElementWebId was null or undefined"))
			}
			if (is.vector(referencedElementWebId) == FALSE) {
				return (print(paste0("Error: referencedElementWebId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/referencedelements'), collapse = "")
			res <- deleteHttpRequest(localVarPath, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getSecurity = function(webId, userIdentity, forceRefresh, selectedFields) {
			queryParameters <- generateListForQueryString(userIdentity, "userIdentity")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(userIdentity) || userIdentity == "") {
				return (paste0("Error: required parameter userIdentity was null or undefined"))
			}
			if (is.vector(userIdentity) == FALSE) {
				return (print(paste0("Error: userIdentity must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/security'), collapse = "")
			if (missing(forceRefresh) == FALSE && is.null(forceRefresh) == FALSE && forceRefresh != "") {
				queryParameters$forceRefresh <- forceRefresh
				if (is.logical(forceRefresh) == FALSE) {
					return (print(paste0("Error: forceRefresh must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsSecurityRights"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 401) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 502) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getSecurityEntries = function(webId, nameFilter, selectedFields) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/securityentries'), collapse = "")
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsSecurityEntry"
			}
			return (contentResponse)
		},
		createSecurityEntry = function(webId, PISecurityEntry, applyToChildren) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PISecurityEntry) || PISecurityEntry == "") {
				return (paste0("Error: required parameter PISecurityEntry was null or undefined"))
			}
			className <- attr(PISecurityEntry, "className")
			if ((is.null(className)) || (className != "PISecurityEntry")) {
				return (print(paste0("Error: the class from the parameter PISecurityEntry should be PISecurityEntry.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/securityentries'), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				queryParameters$applyToChildren <- applyToChildren
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			res <- postHttpRequest(localVarPath, PISecurityEntry, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getSecurityEntryByName = function(name, webId, selectedFields) {
			queryParameters <- list()
			if (is.null(name) || name == "") {
				return (paste0("Error: required parameter name was null or undefined"))
			}
			if (is.character(name) == FALSE) {
				return (print(paste0("Error: name must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/securityentries/', name), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PISecurityEntry"
			}
			if (res$status == 404) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateSecurityEntry = function(name, webId, PISecurityEntry, applyToChildren) {
			queryParameters <- list()
			if (is.null(name) || name == "") {
				return (paste0("Error: required parameter name was null or undefined"))
			}
			if (is.character(name) == FALSE) {
				return (print(paste0("Error: name must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PISecurityEntry) || PISecurityEntry == "") {
				return (paste0("Error: required parameter PISecurityEntry was null or undefined"))
			}
			className <- attr(PISecurityEntry, "className")
			if ((is.null(className)) || (className != "PISecurityEntry")) {
				return (print(paste0("Error: the class from the parameter PISecurityEntry should be PISecurityEntry.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/securityentries/', name), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				queryParameters$applyToChildren <- applyToChildren
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			res <- putHttpRequest(localVarPath, queryParameters, PISecurityEntry, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		deleteSecurityEntry = function(name, webId, applyToChildren) {
			queryParameters <- list()
			if (is.null(name) || name == "") {
				return (paste0("Error: required parameter name was null or undefined"))
			}
			if (is.character(name) == FALSE) {
				return (print(paste0("Error: name must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/', webId, '/securityentries/', name), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				queryParameters$applyToChildren <- applyToChildren
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			res <- deleteHttpRequest(localVarPath, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getMultiple = function(asParallel, includeMode, path, selectedFields, webId) {
			queryParameters <- generateListForTwoQueryString(path, "path", webId, "webId")
			localVarPath <- paste(c(self$serviceBase, '/elements/multiple'), collapse = "")
			if (missing(asParallel) == FALSE && is.null(asParallel) == FALSE && asParallel != "") {
				queryParameters$asParallel <- asParallel
				if (is.logical(asParallel) == FALSE) {
					return (print(paste0("Error: asParallel must be a boolean.")))
				}
			}
			if (missing(includeMode) == FALSE && is.null(includeMode) == FALSE && includeMode != "") {
				queryParameters$includeMode <- includeMode
				if (is.character(includeMode) == FALSE) {
					return (print(paste0("Error: includeMode must be a string.")))
				}
			}
			if (missing(path) == FALSE && is.null(path) == FALSE && path != "") {
				queryParameters$path <- path
				if (is.vector(path) == FALSE) {
					return (print(paste0("Error: path must be a vector.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webId) == FALSE && is.null(webId) == FALSE && webId != "") {
				queryParameters$webId <- webId
				if (is.vector(webId) == FALSE) {
					return (print(paste0("Error: webId must be a vector.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsItemElement"
			}
			if (res$status == 207) {
				attr(contentResponse, "className") <- "PIItemsItemElement"
			}
			return (contentResponse)
		},
		createSearchByAttribute = function() {
			queryParameters <- list()
			localVarPath <- paste(c(self$serviceBase, '/elements/searchbyattribute'), collapse = "")
			res <- postHttpRequest(localVarPath, , self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		executeSearchByAttribute = function(searchId, categoryName, descriptionFilter, maxCount, nameFilter, searchFullHierarchy, selectedFields, sortField, sortOrder, startIndex) {
			queryParameters <- list()
			if (is.null(searchId) || searchId == "") {
				return (paste0("Error: required parameter searchId was null or undefined"))
			}
			if (is.character(searchId) == FALSE) {
				return (print(paste0("Error: searchId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/elements/searchbyattribute/', searchId), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(descriptionFilter) == FALSE && is.null(descriptionFilter) == FALSE && descriptionFilter != "") {
				queryParameters$descriptionFilter <- descriptionFilter
				if (is.character(descriptionFilter) == FALSE) {
					return (print(paste0("Error: descriptionFilter must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				queryParameters$searchFullHierarchy <- searchFullHierarchy
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				queryParameters$sortField <- sortField
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				queryParameters$startIndex <- startIndex
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		}
	)
)
