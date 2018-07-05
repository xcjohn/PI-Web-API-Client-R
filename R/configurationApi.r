configurationApi <- R6Class("configurationApi",
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
		list = function() {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/system/configuration'), collapse = "")
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
			}
			return (contentResponse)
		},
		get = function(key) {
			qs <- customQueryString$new()
			if (is.null(key) || key == "") {
				return (paste0("Error: required parameter key was null or undefined"))
			}
			if (is.character(key) == FALSE) {
				return (print(paste0("Error: key must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/system/configuration/', key), collapse = "")
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
			}
			if (res$status == 404) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		delete = function(key) {
			qs <- customQueryString$new()
			if (is.null(key) || key == "") {
				return (paste0("Error: required parameter key was null or undefined"))
			}
			if (is.character(key) == FALSE) {
				return (print(paste0("Error: key must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/system/configuration/', key), collapse = "")
			res <- deleteHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		}
	)
)
