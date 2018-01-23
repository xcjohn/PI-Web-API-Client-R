assetDatabaseApi <- R6Class("assetDatabaseApi",
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
		getByPath = function(path, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(path) || path == "") {
				return (paste0("Error: required parameter path was null or undefined"))
			}
			if (is.character(path) == FALSE) {
				return (print(paste0("Error: path must be a string.")))
			}
			queryParameters$path <- path
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIAssetDatabase"
			}
			return (contentResponse)
		},
		get = function(webId, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIAssetDatabase"
			}
			return (contentResponse)
		},
		update = function(webId, PIAssetDatabase) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAssetDatabase) || PIAssetDatabase == "") {
				return (paste0("Error: required parameter PIAssetDatabase was null or undefined"))
			}
			className <- attr(PIAssetDatabase, "className")
			if ((is.null(className)) || (className != "PIAssetDatabase")) {
				return (print(paste0("Error: the class from the parameter PIAssetDatabase should be PIAssetDatabase.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId), collapse = "")
			res <- patchHttpRequest(localVarPath, PIAssetDatabase, self$username, self$password, self$authType, self$validateSSL, self$debug)
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
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId), collapse = "")
			res <- deleteHttpRequest(localVarPath, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		findAnalyses = function(webId, field, maxCount, query, selectedFields, sortField, sortOrder, startIndex, webIdType) {
			queryParameters <- generateListForQueryString(field, "field")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(field) || field == "") {
				return (paste0("Error: required parameter field was null or undefined"))
			}
			if (is.vector(field) == FALSE) {
				return (print(paste0("Error: field must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/analyses'), collapse = "")
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(query) == FALSE && is.null(query) == FALSE && query != "") {
				queryParameters$query <- query
				if (is.character(query) == FALSE) {
					return (print(paste0("Error: query must be a string.")))
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAnalysis"
			}
			return (contentResponse)
		},
		getAnalysisCategories = function(webId, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/analysiscategories'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAnalysisCategory"
			}
			return (contentResponse)
		},
		createAnalysisCategory = function(webId, PIAnalysisCategory, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAnalysisCategory) || PIAnalysisCategory == "") {
				return (paste0("Error: required parameter PIAnalysisCategory was null or undefined"))
			}
			className <- attr(PIAnalysisCategory, "className")
			if ((is.null(className)) || (className != "PIAnalysisCategory")) {
				return (print(paste0("Error: the class from the parameter PIAnalysisCategory should be PIAnalysisCategory.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/analysiscategories'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PIAnalysisCategory, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getAnalysisTemplates = function(webId, field, maxCount, query, selectedFields, sortField, sortOrder, webIdType) {
			queryParameters <- generateListForQueryString(field, "field")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(field) || field == "") {
				return (paste0("Error: required parameter field was null or undefined"))
			}
			if (is.vector(field) == FALSE) {
				return (print(paste0("Error: field must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/analysistemplates'), collapse = "")
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(query) == FALSE && is.null(query) == FALSE && query != "") {
				queryParameters$query <- query
				if (is.character(query) == FALSE) {
					return (print(paste0("Error: query must be a string.")))
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAnalysisTemplate"
			}
			return (contentResponse)
		},
		createAnalysisTemplate = function(webId, PIAnalysisTemplate, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAnalysisTemplate) || PIAnalysisTemplate == "") {
				return (paste0("Error: required parameter PIAnalysisTemplate was null or undefined"))
			}
			className <- attr(PIAnalysisTemplate, "className")
			if ((is.null(className)) || (className != "PIAnalysisTemplate")) {
				return (print(paste0("Error: the class from the parameter PIAnalysisTemplate should be PIAnalysisTemplate.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/analysistemplates'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PIAnalysisTemplate, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getAttributeCategories = function(webId, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/attributecategories'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAttributeCategory"
			}
			return (contentResponse)
		},
		createAttributeCategory = function(webId, PIAttributeCategory, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAttributeCategory) || PIAttributeCategory == "") {
				return (paste0("Error: required parameter PIAttributeCategory was null or undefined"))
			}
			className <- attr(PIAttributeCategory, "className")
			if ((is.null(className)) || (className != "PIAttributeCategory")) {
				return (print(paste0("Error: the class from the parameter PIAttributeCategory should be PIAttributeCategory.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/attributecategories'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PIAttributeCategory, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		findElementAttributes = function(webId, attributeCategory, attributeDescriptionFilter, attributeNameFilter, attributeType, elementCategory, elementDescriptionFilter, elementNameFilter, elementTemplate, elementType, maxCount, searchFullHierarchy, selectedFields, sortField, sortOrder, startIndex, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/elementattributes'), collapse = "")
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAttribute"
			}
			return (contentResponse)
		},
		getElementCategories = function(webId, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/elementcategories'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsElementCategory"
			}
			return (contentResponse)
		},
		createElementCategory = function(webId, PIElementCategory, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIElementCategory) || PIElementCategory == "") {
				return (paste0("Error: required parameter PIElementCategory was null or undefined"))
			}
			className <- attr(PIElementCategory, "className")
			if ((is.null(className)) || (className != "PIElementCategory")) {
				return (print(paste0("Error: the class from the parameter PIElementCategory should be PIElementCategory.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/elementcategories'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PIElementCategory, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getElements = function(webId, categoryName, descriptionFilter, elementType, maxCount, nameFilter, searchFullHierarchy, selectedFields, sortField, sortOrder, startIndex, templateName, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/elements'), collapse = "")
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsElement"
			}
			return (contentResponse)
		},
		createElement = function(webId, PIElement, webIdType) {
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
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/elements'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PIElement, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getElementTemplates = function(webId, field, maxCount, query, selectedFields, sortField, sortOrder, webIdType) {
			queryParameters <- generateListForQueryString(field, "field")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(field) || field == "") {
				return (paste0("Error: required parameter field was null or undefined"))
			}
			if (is.vector(field) == FALSE) {
				return (print(paste0("Error: field must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/elementtemplates'), collapse = "")
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(query) == FALSE && is.null(query) == FALSE && query != "") {
				queryParameters$query <- query
				if (is.character(query) == FALSE) {
					return (print(paste0("Error: query must be a string.")))
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsElementTemplate"
			}
			return (contentResponse)
		},
		createElementTemplate = function(webId, PIElementTemplate, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIElementTemplate) || PIElementTemplate == "") {
				return (paste0("Error: required parameter PIElementTemplate was null or undefined"))
			}
			className <- attr(PIElementTemplate, "className")
			if ((is.null(className)) || (className != "PIElementTemplate")) {
				return (print(paste0("Error: the class from the parameter PIElementTemplate should be PIElementTemplate.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/elementtemplates'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PIElementTemplate, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getEnumerationSets = function(webId, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/enumerationsets'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsEnumerationSet"
			}
			return (contentResponse)
		},
		createEnumerationSet = function(webId, PIEnumerationSet, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIEnumerationSet) || PIEnumerationSet == "") {
				return (paste0("Error: required parameter PIEnumerationSet was null or undefined"))
			}
			className <- attr(PIEnumerationSet, "className")
			if ((is.null(className)) || (className != "PIEnumerationSet")) {
				return (print(paste0("Error: the class from the parameter PIEnumerationSet should be PIEnumerationSet.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/enumerationsets'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PIEnumerationSet, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		findEventFrameAttributes = function(webId, attributeCategory, attributeDescriptionFilter, attributeNameFilter, attributeType, endTime, eventFrameCategory, eventFrameDescriptionFilter, eventFrameNameFilter, eventFrameTemplate, maxCount, referencedElementNameFilter, searchFullHierarchy, searchMode, selectedFields, sortField, sortOrder, startIndex, startTime, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/eventframeattributes'), collapse = "")
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
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				queryParameters$endTime <- endTime
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(eventFrameCategory) == FALSE && is.null(eventFrameCategory) == FALSE && eventFrameCategory != "") {
				queryParameters$eventFrameCategory <- eventFrameCategory
				if (is.character(eventFrameCategory) == FALSE) {
					return (print(paste0("Error: eventFrameCategory must be a string.")))
				}
			}
			if (missing(eventFrameDescriptionFilter) == FALSE && is.null(eventFrameDescriptionFilter) == FALSE && eventFrameDescriptionFilter != "") {
				queryParameters$eventFrameDescriptionFilter <- eventFrameDescriptionFilter
				if (is.character(eventFrameDescriptionFilter) == FALSE) {
					return (print(paste0("Error: eventFrameDescriptionFilter must be a string.")))
				}
			}
			if (missing(eventFrameNameFilter) == FALSE && is.null(eventFrameNameFilter) == FALSE && eventFrameNameFilter != "") {
				queryParameters$eventFrameNameFilter <- eventFrameNameFilter
				if (is.character(eventFrameNameFilter) == FALSE) {
					return (print(paste0("Error: eventFrameNameFilter must be a string.")))
				}
			}
			if (missing(eventFrameTemplate) == FALSE && is.null(eventFrameTemplate) == FALSE && eventFrameTemplate != "") {
				queryParameters$eventFrameTemplate <- eventFrameTemplate
				if (is.character(eventFrameTemplate) == FALSE) {
					return (print(paste0("Error: eventFrameTemplate must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				queryParameters$maxCount <- maxCount
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(referencedElementNameFilter) == FALSE && is.null(referencedElementNameFilter) == FALSE && referencedElementNameFilter != "") {
				queryParameters$referencedElementNameFilter <- referencedElementNameFilter
				if (is.character(referencedElementNameFilter) == FALSE) {
					return (print(paste0("Error: referencedElementNameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				queryParameters$searchFullHierarchy <- searchFullHierarchy
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAttribute"
			}
			return (contentResponse)
		},
		getEventFrames = function(webId, canBeAcknowledged, categoryName, endTime, isAcknowledged, maxCount, nameFilter, referencedElementNameFilter, referencedElementTemplateName, searchFullHierarchy, searchMode, selectedFields, severity, sortField, sortOrder, startIndex, startTime, templateName, webIdType) {
			queryParameters <- generateListForQueryString(severity, "severity")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/eventframes'), collapse = "")
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
			if (missing(referencedElementNameFilter) == FALSE && is.null(referencedElementNameFilter) == FALSE && referencedElementNameFilter != "") {
				queryParameters$referencedElementNameFilter <- referencedElementNameFilter
				if (is.character(referencedElementNameFilter) == FALSE) {
					return (print(paste0("Error: referencedElementNameFilter must be a string.")))
				}
			}
			if (missing(referencedElementTemplateName) == FALSE && is.null(referencedElementTemplateName) == FALSE && referencedElementTemplateName != "") {
				queryParameters$referencedElementTemplateName <- referencedElementTemplateName
				if (is.character(referencedElementTemplateName) == FALSE) {
					return (print(paste0("Error: referencedElementTemplateName must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				queryParameters$searchFullHierarchy <- searchFullHierarchy
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsEventFrame"
			}
			return (contentResponse)
		},
		createEventFrame = function(webId, PIEventFrame, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIEventFrame) || PIEventFrame == "") {
				return (paste0("Error: required parameter PIEventFrame was null or undefined"))
			}
			className <- attr(PIEventFrame, "className")
			if ((is.null(className)) || (className != "PIEventFrame")) {
				return (print(paste0("Error: the class from the parameter PIEventFrame should be PIEventFrame.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/eventframes'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PIEventFrame, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		export = function(webId, endTime, exportMode, startTime) {
			queryParameters <- generateListForQueryString(exportMode, "exportMode")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/export'), collapse = "")
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				queryParameters$endTime <- endTime
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(exportMode) == FALSE && is.null(exportMode) == FALSE && exportMode != "") {
				queryParameters$exportMode <- exportMode
				if (is.vector(exportMode) == FALSE) {
					return (print(paste0("Error: exportMode must be a vector.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				queryParameters$startTime <- startTime
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
			}
			return (contentResponse)
		},
		import = function(webId, importMode) {
			queryParameters <- generateListForQueryString(importMode, "importMode")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/import'), collapse = "")
			if (missing(importMode) == FALSE && is.null(importMode) == FALSE && importMode != "") {
				queryParameters$importMode <- importMode
				if (is.vector(importMode) == FALSE) {
					return (print(paste0("Error: importMode must be a vector.")))
				}
			}
			res <- postHttpRequest(localVarPath, , self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getReferencedElements = function(webId, categoryName, descriptionFilter, elementType, maxCount, nameFilter, selectedFields, sortField, sortOrder, startIndex, templateName, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/referencedelements'), collapse = "")
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
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
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/referencedelements'), collapse = "")
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
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/referencedelements'), collapse = "")
			res <- deleteHttpRequest(localVarPath, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getSecurity = function(webId, securityItem, userIdentity, forceRefresh, selectedFields, webIdType) {
			queryParameters <- generateListForTwoQueryString(securityItem, "securityItem", userIdentity, "userIdentity")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(securityItem) || securityItem == "") {
				return (paste0("Error: required parameter securityItem was null or undefined"))
			}
			if (is.vector(securityItem) == FALSE) {
				return (print(paste0("Error: securityItem must be a vector.")))
			}
			if (is.null(userIdentity) || userIdentity == "") {
				return (paste0("Error: required parameter userIdentity was null or undefined"))
			}
			if (is.vector(userIdentity) == FALSE) {
				return (print(paste0("Error: userIdentity must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/security'), collapse = "")
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
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
		getSecurityEntries = function(webId, nameFilter, securityItem, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/securityentries'), collapse = "")
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(securityItem) == FALSE && is.null(securityItem) == FALSE && securityItem != "") {
				queryParameters$securityItem <- securityItem
				if (is.character(securityItem) == FALSE) {
					return (print(paste0("Error: securityItem must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsSecurityEntry"
			}
			return (contentResponse)
		},
		createSecurityEntry = function(webId, PISecurityEntry, applyToChildren, securityItem, webIdType) {
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
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/securityentries'), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				queryParameters$applyToChildren <- applyToChildren
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			if (missing(securityItem) == FALSE && is.null(securityItem) == FALSE && securityItem != "") {
				queryParameters$securityItem <- securityItem
				if (is.character(securityItem) == FALSE) {
					return (print(paste0("Error: securityItem must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PISecurityEntry, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getSecurityEntryByName = function(name, webId, securityItem, selectedFields, webIdType) {
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
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/securityentries/', name), collapse = "")
			if (missing(securityItem) == FALSE && is.null(securityItem) == FALSE && securityItem != "") {
				queryParameters$securityItem <- securityItem
				if (is.character(securityItem) == FALSE) {
					return (print(paste0("Error: securityItem must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
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
		updateSecurityEntry = function(name, webId, PISecurityEntry, applyToChildren, securityItem) {
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
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/securityentries/', name), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				queryParameters$applyToChildren <- applyToChildren
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			if (missing(securityItem) == FALSE && is.null(securityItem) == FALSE && securityItem != "") {
				queryParameters$securityItem <- securityItem
				if (is.character(securityItem) == FALSE) {
					return (print(paste0("Error: securityItem must be a string.")))
				}
			}
			res <- putHttpRequest(localVarPath, queryParameters, PISecurityEntry, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		deleteSecurityEntry = function(name, webId, applyToChildren, securityItem) {
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
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/securityentries/', name), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				queryParameters$applyToChildren <- applyToChildren
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			if (missing(securityItem) == FALSE && is.null(securityItem) == FALSE && securityItem != "") {
				queryParameters$securityItem <- securityItem
				if (is.character(securityItem) == FALSE) {
					return (print(paste0("Error: securityItem must be a string.")))
				}
			}
			res <- deleteHttpRequest(localVarPath, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getTableCategories = function(webId, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/tablecategories'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsTableCategory"
			}
			return (contentResponse)
		},
		createTableCategory = function(webId, PITableCategory, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PITableCategory) || PITableCategory == "") {
				return (paste0("Error: required parameter PITableCategory was null or undefined"))
			}
			className <- attr(PITableCategory, "className")
			if ((is.null(className)) || (className != "PITableCategory")) {
				return (print(paste0("Error: the class from the parameter PITableCategory should be PITableCategory.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/tablecategories'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PITableCategory, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getTables = function(webId, selectedFields, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/tables'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsTable"
			}
			return (contentResponse)
		},
		createTable = function(webId, PITable, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PITable) || PITable == "") {
				return (paste0("Error: required parameter PITable was null or undefined"))
			}
			className <- attr(PITable, "className")
			if ((is.null(className)) || (className != "PITable")) {
				return (print(paste0("Error: the class from the parameter PITable should be PITable.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/assetdatabases/', webId, '/tables'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PITable, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		}
	)
)
