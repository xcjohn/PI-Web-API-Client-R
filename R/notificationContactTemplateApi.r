notificationContactTemplateApi <- R6Class("notificationContactTemplateApi",
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
			localVarPath <- paste(c(self$serviceBase, '/notificationcontacttemplates/', webId), collapse = "")
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
				attr(contentResponse, "className") <- "PINotificationContactTemplate"
			}
			return (contentResponse)
		}
	)
)
