streamSetApi <- R6Class("streamSetApi",
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
		getChannel = function(webId, categoryName, includeInitialValues, nameFilter, searchFullHierarchy, showExcluded, showHidden, templateName) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/channel'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(includeInitialValues) == FALSE && is.null(includeInitialValues) == FALSE && includeInitialValues != "") {
				queryParameters$includeInitialValues <- includeInitialValues
				if (is.logical(includeInitialValues) == FALSE) {
					return (print(paste0("Error: includeInitialValues must be a boolean.")))
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
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 101) {
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getEnd = function(webId, categoryName, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, templateName) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/end'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
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
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getInterpolated = function(webId, categoryName, endTime, filterExpression, includeFilteredValues, interval, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, startTime, templateName, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/interpolated'), collapse = "")
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
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				queryParameters$filterExpression <- filterExpression
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				queryParameters$includeFilteredValues <- includeFilteredValues
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(interval) == FALSE && is.null(interval) == FALSE && interval != "") {
				queryParameters$interval <- interval
				if (is.character(interval) == FALSE) {
					return (print(paste0("Error: interval must be a string.")))
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
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			return (contentResponse)
		},
		getInterpolatedAtTimes = function(webId, time, categoryName, filterExpression, includeFilteredValues, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortOrder, templateName, timeZone) {
			queryParameters <- generateListForQueryString(time, "time")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.vector(time) == FALSE) {
				return (print(paste0("Error: time must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/interpolatedattimes'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				queryParameters$filterExpression <- filterExpression
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				queryParameters$includeFilteredValues <- includeFilteredValues
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
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
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 502) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getPlot = function(webId, categoryName, endTime, intervals, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, startTime, templateName, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/plot'), collapse = "")
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
			if (missing(intervals) == FALSE && is.null(intervals) == FALSE && intervals != "") {
				queryParameters$intervals <- intervals
				if (check.integer(intervals) == FALSE) {
					return (print(paste0("Error: intervals must be an integer.")))
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
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecorded = function(webId, boundaryType, categoryName, endTime, filterExpression, includeFilteredValues, maxCount, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, startTime, templateName, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/recorded'), collapse = "")
			if (missing(boundaryType) == FALSE && is.null(boundaryType) == FALSE && boundaryType != "") {
				queryParameters$boundaryType <- boundaryType
				if (is.character(boundaryType) == FALSE) {
					return (print(paste0("Error: boundaryType must be a string.")))
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
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				queryParameters$filterExpression <- filterExpression
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				queryParameters$includeFilteredValues <- includeFilteredValues
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
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
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValues = function(webId, values, bufferOption, updateOption) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(values) || values == "") {
				return (paste0("Error: required parameter values was null or undefined"))
			}
			if (is.vector(values) == FALSE) {
				return (print(paste0("Error: values must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/recorded'), collapse = "")
			if (missing(bufferOption) == FALSE && is.null(bufferOption) == FALSE && bufferOption != "") {
				queryParameters$bufferOption <- bufferOption
				if (is.character(bufferOption) == FALSE) {
					return (print(paste0("Error: bufferOption must be a string.")))
				}
			}
			if (missing(updateOption) == FALSE && is.null(updateOption) == FALSE && updateOption != "") {
				queryParameters$updateOption <- updateOption
				if (is.character(updateOption) == FALSE) {
					return (print(paste0("Error: updateOption must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, values, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getRecordedAtTime = function(webId, time, categoryName, nameFilter, retrievalMode, searchFullHierarchy, selectedFields, showExcluded, showHidden, templateName, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.character(time) == FALSE) {
				return (print(paste0("Error: time must be a string.")))
			}
			queryParameters$time <- time
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/recordedattime'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(retrievalMode) == FALSE && is.null(retrievalMode) == FALSE && retrievalMode != "") {
				queryParameters$retrievalMode <- retrievalMode
				if (is.character(retrievalMode) == FALSE) {
					return (print(paste0("Error: retrievalMode must be a string.")))
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
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecordedAtTimes = function(webId, time, categoryName, nameFilter, retrievalMode, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortOrder, templateName, timeZone) {
			queryParameters <- generateListForQueryString(time, "time")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.vector(time) == FALSE) {
				return (print(paste0("Error: time must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/recordedattimes'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(retrievalMode) == FALSE && is.null(retrievalMode) == FALSE && retrievalMode != "") {
				queryParameters$retrievalMode <- retrievalMode
				if (is.character(retrievalMode) == FALSE) {
					return (print(paste0("Error: retrievalMode must be a string.")))
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
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 502) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getSummaries = function(webId, calculationBasis, categoryName, endTime, filterExpression, nameFilter, sampleInterval, sampleType, searchFullHierarchy, selectedFields, showExcluded, showHidden, startTime, summaryDuration, summaryType, templateName, timeType, timeZone) {
			queryParameters <- generateListForQueryString(summaryType, "summaryType")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/summary'), collapse = "")
			if (missing(calculationBasis) == FALSE && is.null(calculationBasis) == FALSE && calculationBasis != "") {
				queryParameters$calculationBasis <- calculationBasis
				if (is.character(calculationBasis) == FALSE) {
					return (print(paste0("Error: calculationBasis must be a string.")))
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
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				queryParameters$filterExpression <- filterExpression
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				queryParameters$nameFilter <- nameFilter
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(sampleInterval) == FALSE && is.null(sampleInterval) == FALSE && sampleInterval != "") {
				queryParameters$sampleInterval <- sampleInterval
				if (is.character(sampleInterval) == FALSE) {
					return (print(paste0("Error: sampleInterval must be a string.")))
				}
			}
			if (missing(sampleType) == FALSE && is.null(sampleType) == FALSE && sampleType != "") {
				queryParameters$sampleType <- sampleType
				if (is.character(sampleType) == FALSE) {
					return (print(paste0("Error: sampleType must be a string.")))
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
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				queryParameters$startTime <- startTime
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(summaryDuration) == FALSE && is.null(summaryDuration) == FALSE && summaryDuration != "") {
				queryParameters$summaryDuration <- summaryDuration
				if (is.character(summaryDuration) == FALSE) {
					return (print(paste0("Error: summaryDuration must be a string.")))
				}
			}
			if (missing(summaryType) == FALSE && is.null(summaryType) == FALSE && summaryType != "") {
				queryParameters$summaryType <- summaryType
				if (is.vector(summaryType) == FALSE) {
					return (print(paste0("Error: summaryType must be a vector.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeType) == FALSE && is.null(timeType) == FALSE && timeType != "") {
				queryParameters$timeType <- timeType
				if (is.character(timeType) == FALSE) {
					return (print(paste0("Error: timeType must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamSummaries"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getValues = function(webId, categoryName, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, templateName, time, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/value'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				queryParameters$categoryName <- categoryName
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
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
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				queryParameters$templateName <- templateName
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(time) == FALSE && is.null(time) == FALSE && time != "") {
				queryParameters$time <- time
				if (is.character(time) == FALSE) {
					return (print(paste0("Error: time must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValue = function(webId, values, bufferOption, updateOption) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(values) || values == "") {
				return (paste0("Error: required parameter values was null or undefined"))
			}
			if (is.vector(values) == FALSE) {
				return (print(paste0("Error: values must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/value'), collapse = "")
			if (missing(bufferOption) == FALSE && is.null(bufferOption) == FALSE && bufferOption != "") {
				queryParameters$bufferOption <- bufferOption
				if (is.character(bufferOption) == FALSE) {
					return (print(paste0("Error: bufferOption must be a string.")))
				}
			}
			if (missing(updateOption) == FALSE && is.null(updateOption) == FALSE && updateOption != "") {
				queryParameters$updateOption <- updateOption
				if (is.character(updateOption) == FALSE) {
					return (print(paste0("Error: updateOption must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, values, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getChannelAdHoc = function(webId, includeInitialValues) {
			queryParameters <- generateListForQueryString(webId, "webId")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/channel'), collapse = "")
			if (missing(includeInitialValues) == FALSE && is.null(includeInitialValues) == FALSE && includeInitialValues != "") {
				queryParameters$includeInitialValues <- includeInitialValues
				if (is.logical(includeInitialValues) == FALSE) {
					return (print(paste0("Error: includeInitialValues must be a boolean.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 101) {
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			return (contentResponse)
		},
		getEndAdHoc = function(webId, selectedFields) {
			queryParameters <- generateListForQueryString(webId, "webId")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/end'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			return (contentResponse)
		},
		getInterpolatedAdHoc = function(webId, endTime, filterExpression, includeFilteredValues, interval, selectedFields, startTime, timeZone) {
			queryParameters <- generateListForQueryString(webId, "webId")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/interpolated'), collapse = "")
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				queryParameters$endTime <- endTime
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				queryParameters$filterExpression <- filterExpression
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				queryParameters$includeFilteredValues <- includeFilteredValues
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(interval) == FALSE && is.null(interval) == FALSE && interval != "") {
				queryParameters$interval <- interval
				if (is.character(interval) == FALSE) {
					return (print(paste0("Error: interval must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				queryParameters$startTime <- startTime
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			return (contentResponse)
		},
		getInterpolatedAtTimesAdHoc = function(time, webId, filterExpression, includeFilteredValues, selectedFields, sortOrder, timeZone) {
			queryParameters <- generateListForTwoQueryString(time, "time", webId, "webId")
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.vector(time) == FALSE) {
				return (print(paste0("Error: time must be a vector.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/interpolatedattimes'), collapse = "")
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				queryParameters$filterExpression <- filterExpression
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				queryParameters$includeFilteredValues <- includeFilteredValues
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 502) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getPlotAdHoc = function(webId, endTime, intervals, selectedFields, startTime, timeZone) {
			queryParameters <- generateListForQueryString(webId, "webId")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/plot'), collapse = "")
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				queryParameters$endTime <- endTime
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(intervals) == FALSE && is.null(intervals) == FALSE && intervals != "") {
				queryParameters$intervals <- intervals
				if (check.integer(intervals) == FALSE) {
					return (print(paste0("Error: intervals must be an integer.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				queryParameters$startTime <- startTime
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecordedAdHoc = function(webId, boundaryType, endTime, filterExpression, includeFilteredValues, maxCount, selectedFields, startTime, timeZone) {
			queryParameters <- generateListForQueryString(webId, "webId")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/recorded'), collapse = "")
			if (missing(boundaryType) == FALSE && is.null(boundaryType) == FALSE && boundaryType != "") {
				queryParameters$boundaryType <- boundaryType
				if (is.character(boundaryType) == FALSE) {
					return (print(paste0("Error: boundaryType must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				queryParameters$endTime <- endTime
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				queryParameters$filterExpression <- filterExpression
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				queryParameters$includeFilteredValues <- includeFilteredValues
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
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
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				queryParameters$startTime <- startTime
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValuesAdHoc = function(values, bufferOption, updateOption) {
			queryParameters <- list()
			if (is.null(values) || values == "") {
				return (paste0("Error: required parameter values was null or undefined"))
			}
			if (is.vector(values) == FALSE) {
				return (print(paste0("Error: values must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/recorded'), collapse = "")
			if (missing(bufferOption) == FALSE && is.null(bufferOption) == FALSE && bufferOption != "") {
				queryParameters$bufferOption <- bufferOption
				if (is.character(bufferOption) == FALSE) {
					return (print(paste0("Error: bufferOption must be a string.")))
				}
			}
			if (missing(updateOption) == FALSE && is.null(updateOption) == FALSE && updateOption != "") {
				queryParameters$updateOption <- updateOption
				if (is.character(updateOption) == FALSE) {
					return (print(paste0("Error: updateOption must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, values, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getRecordedAtTimeAdHoc = function(time, webId, retrievalMode, selectedFields, timeZone) {
			queryParameters <- generateListForQueryString(webId, "webId")
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.character(time) == FALSE) {
				return (print(paste0("Error: time must be a string.")))
			}
			queryParameters$time <- time
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/recordedattime'), collapse = "")
			if (missing(retrievalMode) == FALSE && is.null(retrievalMode) == FALSE && retrievalMode != "") {
				queryParameters$retrievalMode <- retrievalMode
				if (is.character(retrievalMode) == FALSE) {
					return (print(paste0("Error: retrievalMode must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecordedAtTimesAdHoc = function(time, webId, retrievalMode, selectedFields, sortOrder, timeZone) {
			queryParameters <- generateListForTwoQueryString(time, "time", webId, "webId")
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.vector(time) == FALSE) {
				return (print(paste0("Error: time must be a vector.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/recordedattimes'), collapse = "")
			if (missing(retrievalMode) == FALSE && is.null(retrievalMode) == FALSE && retrievalMode != "") {
				queryParameters$retrievalMode <- retrievalMode
				if (is.character(retrievalMode) == FALSE) {
					return (print(paste0("Error: retrievalMode must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				queryParameters$sortOrder <- sortOrder
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 502) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getSummariesAdHoc = function(webId, calculationBasis, endTime, filterExpression, sampleInterval, sampleType, selectedFields, startTime, summaryDuration, summaryType, timeType, timeZone) {
			queryParameters <- generateListForTwoQueryString(webId, "webId", summaryType, "summaryType")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/summary'), collapse = "")
			if (missing(calculationBasis) == FALSE && is.null(calculationBasis) == FALSE && calculationBasis != "") {
				queryParameters$calculationBasis <- calculationBasis
				if (is.character(calculationBasis) == FALSE) {
					return (print(paste0("Error: calculationBasis must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				queryParameters$endTime <- endTime
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				queryParameters$filterExpression <- filterExpression
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(sampleInterval) == FALSE && is.null(sampleInterval) == FALSE && sampleInterval != "") {
				queryParameters$sampleInterval <- sampleInterval
				if (is.character(sampleInterval) == FALSE) {
					return (print(paste0("Error: sampleInterval must be a string.")))
				}
			}
			if (missing(sampleType) == FALSE && is.null(sampleType) == FALSE && sampleType != "") {
				queryParameters$sampleType <- sampleType
				if (is.character(sampleType) == FALSE) {
					return (print(paste0("Error: sampleType must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				queryParameters$startTime <- startTime
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(summaryDuration) == FALSE && is.null(summaryDuration) == FALSE && summaryDuration != "") {
				queryParameters$summaryDuration <- summaryDuration
				if (is.character(summaryDuration) == FALSE) {
					return (print(paste0("Error: summaryDuration must be a string.")))
				}
			}
			if (missing(summaryType) == FALSE && is.null(summaryType) == FALSE && summaryType != "") {
				queryParameters$summaryType <- summaryType
				if (is.vector(summaryType) == FALSE) {
					return (print(paste0("Error: summaryType must be a vector.")))
				}
			}
			if (missing(timeType) == FALSE && is.null(timeType) == FALSE && timeType != "") {
				queryParameters$timeType <- timeType
				if (is.character(timeType) == FALSE) {
					return (print(paste0("Error: timeType must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamSummaries"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getValuesAdHoc = function(webId, selectedFields, time, timeZone) {
			queryParameters <- generateListForQueryString(webId, "webId")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/value'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				queryParameters$selectedFields <- selectedFields
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(time) == FALSE && is.null(time) == FALSE && time != "") {
				queryParameters$time <- time
				if (is.character(time) == FALSE) {
					return (print(paste0("Error: time must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				queryParameters$timeZone <- timeZone
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, queryParameters, self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValueAdHoc = function(values, bufferOption, updateOption) {
			queryParameters <- list()
			if (is.null(values) || values == "") {
				return (paste0("Error: required parameter values was null or undefined"))
			}
			if (is.vector(values) == FALSE) {
				return (print(paste0("Error: values must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/value'), collapse = "")
			if (missing(bufferOption) == FALSE && is.null(bufferOption) == FALSE && bufferOption != "") {
				queryParameters$bufferOption <- bufferOption
				if (is.character(bufferOption) == FALSE) {
					return (print(paste0("Error: bufferOption must be a string.")))
				}
			}
			if (missing(updateOption) == FALSE && is.null(updateOption) == FALSE && updateOption != "") {
				queryParameters$updateOption <- updateOption
				if (is.character(updateOption) == FALSE) {
					return (print(paste0("Error: updateOption must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, values, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		}
	)
)
