notificationRuleTemplateApi <- R6Class("notificationRuleTemplateApi",
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
		get = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/notificationruletemplates/', webId), collapse = "")
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
				attr(contentResponse, "className") <- "PINotificationRuleTemplate"
			}
			return (contentResponse)
		},
		getNotificationRuleTemplateSubscribers = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/notificationruletemplates/', webId, '/notificationrulesubscribers'), collapse = "")
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
				attr(contentResponse, "className") <- "PIItemsNotificationRuleSubscriber"
			}
			return (contentResponse)
		},
		getNotificationRuleTemplatesQuery = function(databaseWebId, maxCount, query, selectedFields, startIndex, webIdType) {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/notificationruletemplates/search'), collapse = "")
			if (missing(databaseWebId) == FALSE && is.null(databaseWebId) == FALSE && databaseWebId != "") {
				qs$add('databaseWebId', databaseWebId, FALSE);
				if (is.character(databaseWebId) == FALSE) {
					return (print(paste0("Error: databaseWebId must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				qs$add('maxCount', maxCount, FALSE);
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(query) == FALSE && is.null(query) == FALSE && query != "") {
				qs$add('query', query, FALSE);
				if (is.character(query) == FALSE) {
					return (print(paste0("Error: query must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				qs$add('startIndex', startIndex, FALSE);
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
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
				attr(contentResponse, "className") <- "PIItemsNotificationRuleTemplate"
			}
			return (contentResponse)
		}
	)
)
