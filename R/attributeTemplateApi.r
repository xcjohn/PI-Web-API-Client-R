attributeTemplateApi <- R6Class("attributeTemplateApi",
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
			qs <- customQueryString$new()
			if (is.null(path) || path == "") {
				return (paste0("Error: required parameter path was null or undefined"))
			}
			if (is.character(path) == FALSE) {
				return (print(paste0("Error: path must be a string.")))
			}
			qs$add('path', path, FALSE);
			localVarPath <- paste(c(self$serviceBase, '/attributetemplates'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIAttributeTemplate"
			}
			return (contentResponse)
		},
		get = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/attributetemplates/', webId), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIAttributeTemplate"
			}
			return (contentResponse)
		},
		update = function(webId, PIAttributeTemplate) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAttributeTemplate) || PIAttributeTemplate == "") {
				return (paste0("Error: required parameter PIAttributeTemplate was null or undefined"))
			}
			className <- attr(PIAttributeTemplate, "className")
			if ((is.null(className)) || (className != "PIAttributeTemplate")) {
				return (print(paste0("Error: the class from the parameter PIAttributeTemplate should be PIAttributeTemplate.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/attributetemplates/', webId), collapse = "")
			res <- patchHttpRequest(localVarPath, qs$getQueryParameters(), PIAttributeTemplate, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		delete = function(webId) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/attributetemplates/', webId), collapse = "")
			res <- deleteHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getAttributeTemplates = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/attributetemplates/', webId, '/attributetemplates'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAttributeTemplate"
			}
			return (contentResponse)
		},
		createAttributeTemplate = function(webId, PIAttributeTemplate, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAttributeTemplate) || PIAttributeTemplate == "") {
				return (paste0("Error: required parameter PIAttributeTemplate was null or undefined"))
			}
			className <- attr(PIAttributeTemplate, "className")
			if ((is.null(className)) || (className != "PIAttributeTemplate")) {
				return (print(paste0("Error: the class from the parameter PIAttributeTemplate should be PIAttributeTemplate.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/attributetemplates/', webId, '/attributetemplates'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), PIAttributeTemplate, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getCategories = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/attributetemplates/', webId, '/categories'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsAttributeCategory"
			}
			return (contentResponse)
		}
	)
)
