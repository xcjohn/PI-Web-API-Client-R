systemApi <- R6Class("systemApi",
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
		landing = function() {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/system'), collapse = "")
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PISystemLanding"
			}
			return (contentResponse)
		},
		cacheInstances = function() {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/system/cacheinstances'), collapse = "")
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsCacheInstance"
			}
			return (contentResponse)
		},
		status = function() {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/system/status'), collapse = "")
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PISystemStatus"
			}
			return (contentResponse)
		},
		userInfo = function() {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/system/userinfo'), collapse = "")
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIUserInfo"
			}
			if (res$status == 204) {
				attr(contentResponse, "className") <- "PIUserInfo"
			}
			return (contentResponse)
		},
		versions = function() {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/system/versions'), collapse = "")
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
			}
			return (contentResponse)
		}
	)
)
