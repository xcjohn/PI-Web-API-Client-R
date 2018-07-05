batchApi <- R6Class("batchApi",
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
		execute = function(batch) {
			qs <- customQueryString$new()
			if (is.null(batch) || batch == "") {
				return (paste0("Error: required parameter batch was null or undefined"))
			}
			localVarPath <- paste(c(self$serviceBase, '/batch'), collapse = "")
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), batch, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		}
	)
)
