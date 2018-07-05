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
		getChannel = function(webId, categoryName, heartbeatRate, includeInitialValues, nameFilter, searchFullHierarchy, showExcluded, showHidden, templateName, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/channel'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(heartbeatRate) == FALSE && is.null(heartbeatRate) == FALSE && heartbeatRate != "") {
				qs$add('heartbeatRate', heartbeatRate, FALSE);
				if (check.integer(heartbeatRate) == FALSE) {
					return (print(paste0("Error: heartbeatRate must be an integer.")))
				}
			}
			if (missing(includeInitialValues) == FALSE && is.null(includeInitialValues) == FALSE && includeInitialValues != "") {
				qs$add('includeInitialValues', includeInitialValues, FALSE);
				if (is.logical(includeInitialValues) == FALSE) {
					return (print(paste0("Error: includeInitialValues must be a boolean.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
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
			if (res$status == 101) {
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getEnd = function(webId, categoryName, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortField, sortOrder, templateName, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/end'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getInterpolated = function(webId, categoryName, endTime, filterExpression, includeFilteredValues, interval, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortField, sortOrder, startTime, syncTime, syncTimeBoundaryType, templateName, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/interpolated'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				qs$add('filterExpression', filterExpression, FALSE);
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				qs$add('includeFilteredValues', includeFilteredValues, FALSE);
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(interval) == FALSE && is.null(interval) == FALSE && interval != "") {
				qs$add('interval', interval, FALSE);
				if (is.character(interval) == FALSE) {
					return (print(paste0("Error: interval must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(syncTime) == FALSE && is.null(syncTime) == FALSE && syncTime != "") {
				qs$add('syncTime', syncTime, FALSE);
				if (is.character(syncTime) == FALSE) {
					return (print(paste0("Error: syncTime must be a string.")))
				}
			}
			if (missing(syncTimeBoundaryType) == FALSE && is.null(syncTimeBoundaryType) == FALSE && syncTimeBoundaryType != "") {
				qs$add('syncTimeBoundaryType', syncTimeBoundaryType, FALSE);
				if (is.character(syncTimeBoundaryType) == FALSE) {
					return (print(paste0("Error: syncTimeBoundaryType must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			return (contentResponse)
		},
		getInterpolatedAtTimes = function(webId, time, categoryName, filterExpression, includeFilteredValues, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortOrder, templateName, timeZone, webIdType) {
			qs <- customQueryString$new()
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
			qs$add('time', time, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/interpolatedattimes'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				qs$add('filterExpression', filterExpression, FALSE);
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				qs$add('includeFilteredValues', includeFilteredValues, FALSE);
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
		getPlot = function(webId, categoryName, endTime, intervals, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortField, sortOrder, startTime, templateName, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/plot'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(intervals) == FALSE && is.null(intervals) == FALSE && intervals != "") {
				qs$add('intervals', intervals, FALSE);
				if (check.integer(intervals) == FALSE) {
					return (print(paste0("Error: intervals must be an integer.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecorded = function(webId, boundaryType, categoryName, endTime, filterExpression, includeFilteredValues, maxCount, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortField, sortOrder, startTime, templateName, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/recorded'), collapse = "")
			if (missing(boundaryType) == FALSE && is.null(boundaryType) == FALSE && boundaryType != "") {
				qs$add('boundaryType', boundaryType, FALSE);
				if (is.character(boundaryType) == FALSE) {
					return (print(paste0("Error: boundaryType must be a string.")))
				}
			}
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				qs$add('filterExpression', filterExpression, FALSE);
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				qs$add('includeFilteredValues', includeFilteredValues, FALSE);
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				qs$add('maxCount', maxCount, FALSE);
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValues = function(webId, values, bufferOption, updateOption) {
			qs <- customQueryString$new()
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
				qs$add('bufferOption', bufferOption, FALSE);
				if (is.character(bufferOption) == FALSE) {
					return (print(paste0("Error: bufferOption must be a string.")))
				}
			}
			if (missing(updateOption) == FALSE && is.null(updateOption) == FALSE && updateOption != "") {
				qs$add('updateOption', updateOption, FALSE);
				if (is.character(updateOption) == FALSE) {
					return (print(paste0("Error: updateOption must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), values, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getRecordedAtTime = function(webId, time, categoryName, nameFilter, retrievalMode, searchFullHierarchy, selectedFields, showExcluded, showHidden, templateName, timeZone, webIdType) {
			qs <- customQueryString$new()
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
			qs$add('time', time, FALSE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/recordedattime'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(retrievalMode) == FALSE && is.null(retrievalMode) == FALSE && retrievalMode != "") {
				qs$add('retrievalMode', retrievalMode, FALSE);
				if (is.character(retrievalMode) == FALSE) {
					return (print(paste0("Error: retrievalMode must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecordedAtTimes = function(webId, time, categoryName, nameFilter, retrievalMode, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortOrder, templateName, timeZone, webIdType) {
			qs <- customQueryString$new()
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
			qs$add('time', time, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/recordedattimes'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(retrievalMode) == FALSE && is.null(retrievalMode) == FALSE && retrievalMode != "") {
				qs$add('retrievalMode', retrievalMode, FALSE);
				if (is.character(retrievalMode) == FALSE) {
					return (print(paste0("Error: retrievalMode must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
		getSummaries = function(webId, calculationBasis, categoryName, endTime, filterExpression, nameFilter, sampleInterval, sampleType, searchFullHierarchy, selectedFields, showExcluded, showHidden, startTime, summaryDuration, summaryType, templateName, timeType, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/summary'), collapse = "")
			if (missing(calculationBasis) == FALSE && is.null(calculationBasis) == FALSE && calculationBasis != "") {
				qs$add('calculationBasis', calculationBasis, FALSE);
				if (is.character(calculationBasis) == FALSE) {
					return (print(paste0("Error: calculationBasis must be a string.")))
				}
			}
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				qs$add('filterExpression', filterExpression, FALSE);
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(sampleInterval) == FALSE && is.null(sampleInterval) == FALSE && sampleInterval != "") {
				qs$add('sampleInterval', sampleInterval, FALSE);
				if (is.character(sampleInterval) == FALSE) {
					return (print(paste0("Error: sampleInterval must be a string.")))
				}
			}
			if (missing(sampleType) == FALSE && is.null(sampleType) == FALSE && sampleType != "") {
				qs$add('sampleType', sampleType, FALSE);
				if (is.character(sampleType) == FALSE) {
					return (print(paste0("Error: sampleType must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(summaryDuration) == FALSE && is.null(summaryDuration) == FALSE && summaryDuration != "") {
				qs$add('summaryDuration', summaryDuration, FALSE);
				if (is.character(summaryDuration) == FALSE) {
					return (print(paste0("Error: summaryDuration must be a string.")))
				}
			}
			if (missing(summaryType) == FALSE && is.null(summaryType) == FALSE && summaryType != "") {
				qs$add('summaryType', summaryType, TRUE);
				if (is.vector(summaryType) == FALSE) {
					return (print(paste0("Error: summaryType must be a vector.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(timeType) == FALSE && is.null(timeType) == FALSE && timeType != "") {
				qs$add('timeType', timeType, FALSE);
				if (is.character(timeType) == FALSE) {
					return (print(paste0("Error: timeType must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamSummaries"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getValues = function(webId, categoryName, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortField, sortOrder, templateName, time, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/', webId, '/value'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
				}
			}
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(showExcluded) == FALSE && is.null(showExcluded) == FALSE && showExcluded != "") {
				qs$add('showExcluded', showExcluded, FALSE);
				if (is.logical(showExcluded) == FALSE) {
					return (print(paste0("Error: showExcluded must be a boolean.")))
				}
			}
			if (missing(showHidden) == FALSE && is.null(showHidden) == FALSE && showHidden != "") {
				qs$add('showHidden', showHidden, FALSE);
				if (is.logical(showHidden) == FALSE) {
					return (print(paste0("Error: showHidden must be a boolean.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(time) == FALSE && is.null(time) == FALSE && time != "") {
				qs$add('time', time, FALSE);
				if (is.character(time) == FALSE) {
					return (print(paste0("Error: time must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValue = function(webId, values, bufferOption, updateOption) {
			qs <- customQueryString$new()
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
				qs$add('bufferOption', bufferOption, FALSE);
				if (is.character(bufferOption) == FALSE) {
					return (print(paste0("Error: bufferOption must be a string.")))
				}
			}
			if (missing(updateOption) == FALSE && is.null(updateOption) == FALSE && updateOption != "") {
				qs$add('updateOption', updateOption, FALSE);
				if (is.character(updateOption) == FALSE) {
					return (print(paste0("Error: updateOption must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), values, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getChannelAdHoc = function(webId, heartbeatRate, includeInitialValues, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/channel'), collapse = "")
			if (missing(heartbeatRate) == FALSE && is.null(heartbeatRate) == FALSE && heartbeatRate != "") {
				qs$add('heartbeatRate', heartbeatRate, FALSE);
				if (check.integer(heartbeatRate) == FALSE) {
					return (print(paste0("Error: heartbeatRate must be an integer.")))
				}
			}
			if (missing(includeInitialValues) == FALSE && is.null(includeInitialValues) == FALSE && includeInitialValues != "") {
				qs$add('includeInitialValues', includeInitialValues, FALSE);
				if (is.logical(includeInitialValues) == FALSE) {
					return (print(paste0("Error: includeInitialValues must be a boolean.")))
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
			if (res$status == 101) {
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			return (contentResponse)
		},
		getEndAdHoc = function(webId, selectedFields, sortField, sortOrder, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/end'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			return (contentResponse)
		},
		getInterpolatedAdHoc = function(webId, endTime, filterExpression, includeFilteredValues, interval, selectedFields, sortField, sortOrder, startTime, syncTime, syncTimeBoundaryType, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/interpolated'), collapse = "")
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				qs$add('filterExpression', filterExpression, FALSE);
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				qs$add('includeFilteredValues', includeFilteredValues, FALSE);
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(interval) == FALSE && is.null(interval) == FALSE && interval != "") {
				qs$add('interval', interval, FALSE);
				if (is.character(interval) == FALSE) {
					return (print(paste0("Error: interval must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(syncTime) == FALSE && is.null(syncTime) == FALSE && syncTime != "") {
				qs$add('syncTime', syncTime, FALSE);
				if (is.character(syncTime) == FALSE) {
					return (print(paste0("Error: syncTime must be a string.")))
				}
			}
			if (missing(syncTimeBoundaryType) == FALSE && is.null(syncTimeBoundaryType) == FALSE && syncTimeBoundaryType != "") {
				qs$add('syncTimeBoundaryType', syncTimeBoundaryType, FALSE);
				if (is.character(syncTimeBoundaryType) == FALSE) {
					return (print(paste0("Error: syncTimeBoundaryType must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			return (contentResponse)
		},
		getInterpolatedAtTimesAdHoc = function(time, webId, filterExpression, includeFilteredValues, selectedFields, sortOrder, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.vector(time) == FALSE) {
				return (print(paste0("Error: time must be a vector.")))
			}
			qs$add('time', time, TRUE);
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/interpolatedattimes'), collapse = "")
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				qs$add('filterExpression', filterExpression, FALSE);
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				qs$add('includeFilteredValues', includeFilteredValues, FALSE);
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
		getPlotAdHoc = function(webId, endTime, intervals, selectedFields, sortField, sortOrder, startTime, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/plot'), collapse = "")
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(intervals) == FALSE && is.null(intervals) == FALSE && intervals != "") {
				qs$add('intervals', intervals, FALSE);
				if (check.integer(intervals) == FALSE) {
					return (print(paste0("Error: intervals must be an integer.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecordedAdHoc = function(webId, boundaryType, endTime, filterExpression, includeFilteredValues, maxCount, selectedFields, sortField, sortOrder, startTime, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/recorded'), collapse = "")
			if (missing(boundaryType) == FALSE && is.null(boundaryType) == FALSE && boundaryType != "") {
				qs$add('boundaryType', boundaryType, FALSE);
				if (is.character(boundaryType) == FALSE) {
					return (print(paste0("Error: boundaryType must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				qs$add('filterExpression', filterExpression, FALSE);
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(includeFilteredValues) == FALSE && is.null(includeFilteredValues) == FALSE && includeFilteredValues != "") {
				qs$add('includeFilteredValues', includeFilteredValues, FALSE);
				if (is.logical(includeFilteredValues) == FALSE) {
					return (print(paste0("Error: includeFilteredValues must be a boolean.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				qs$add('maxCount', maxCount, FALSE);
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValuesAdHoc = function(values, bufferOption, updateOption) {
			qs <- customQueryString$new()
			if (is.null(values) || values == "") {
				return (paste0("Error: required parameter values was null or undefined"))
			}
			if (is.vector(values) == FALSE) {
				return (print(paste0("Error: values must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/recorded'), collapse = "")
			if (missing(bufferOption) == FALSE && is.null(bufferOption) == FALSE && bufferOption != "") {
				qs$add('bufferOption', bufferOption, FALSE);
				if (is.character(bufferOption) == FALSE) {
					return (print(paste0("Error: bufferOption must be a string.")))
				}
			}
			if (missing(updateOption) == FALSE && is.null(updateOption) == FALSE && updateOption != "") {
				qs$add('updateOption', updateOption, FALSE);
				if (is.character(updateOption) == FALSE) {
					return (print(paste0("Error: updateOption must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), values, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getRecordedAtTimeAdHoc = function(time, webId, retrievalMode, selectedFields, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.character(time) == FALSE) {
				return (print(paste0("Error: time must be a string.")))
			}
			qs$add('time', time, FALSE);
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/recordedattime'), collapse = "")
			if (missing(retrievalMode) == FALSE && is.null(retrievalMode) == FALSE && retrievalMode != "") {
				qs$add('retrievalMode', retrievalMode, FALSE);
				if (is.character(retrievalMode) == FALSE) {
					return (print(paste0("Error: retrievalMode must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecordedAtTimesAdHoc = function(time, webId, retrievalMode, selectedFields, sortOrder, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(time) || time == "") {
				return (paste0("Error: required parameter time was null or undefined"))
			}
			if (is.vector(time) == FALSE) {
				return (print(paste0("Error: time must be a vector.")))
			}
			qs$add('time', time, TRUE);
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/recordedattimes'), collapse = "")
			if (missing(retrievalMode) == FALSE && is.null(retrievalMode) == FALSE && retrievalMode != "") {
				qs$add('retrievalMode', retrievalMode, FALSE);
				if (is.character(retrievalMode) == FALSE) {
					return (print(paste0("Error: retrievalMode must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
		getSummariesAdHoc = function(webId, calculationBasis, endTime, filterExpression, sampleInterval, sampleType, selectedFields, startTime, summaryDuration, summaryType, timeType, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/summary'), collapse = "")
			if (missing(calculationBasis) == FALSE && is.null(calculationBasis) == FALSE && calculationBasis != "") {
				qs$add('calculationBasis', calculationBasis, FALSE);
				if (is.character(calculationBasis) == FALSE) {
					return (print(paste0("Error: calculationBasis must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(filterExpression) == FALSE && is.null(filterExpression) == FALSE && filterExpression != "") {
				qs$add('filterExpression', filterExpression, FALSE);
				if (is.character(filterExpression) == FALSE) {
					return (print(paste0("Error: filterExpression must be a string.")))
				}
			}
			if (missing(sampleInterval) == FALSE && is.null(sampleInterval) == FALSE && sampleInterval != "") {
				qs$add('sampleInterval', sampleInterval, FALSE);
				if (is.character(sampleInterval) == FALSE) {
					return (print(paste0("Error: sampleInterval must be a string.")))
				}
			}
			if (missing(sampleType) == FALSE && is.null(sampleType) == FALSE && sampleType != "") {
				qs$add('sampleType', sampleType, FALSE);
				if (is.character(sampleType) == FALSE) {
					return (print(paste0("Error: sampleType must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(summaryDuration) == FALSE && is.null(summaryDuration) == FALSE && summaryDuration != "") {
				qs$add('summaryDuration', summaryDuration, FALSE);
				if (is.character(summaryDuration) == FALSE) {
					return (print(paste0("Error: summaryDuration must be a string.")))
				}
			}
			if (missing(summaryType) == FALSE && is.null(summaryType) == FALSE && summaryType != "") {
				qs$add('summaryType', summaryType, TRUE);
				if (is.vector(summaryType) == FALSE) {
					return (print(paste0("Error: summaryType must be a vector.")))
				}
			}
			if (missing(timeType) == FALSE && is.null(timeType) == FALSE && timeType != "") {
				qs$add('timeType', timeType, FALSE);
				if (is.character(timeType) == FALSE) {
					return (print(paste0("Error: timeType must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamSummaries"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		retrieveStreamSetUpdates = function(marker, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(marker) || marker == "") {
				return (paste0("Error: required parameter marker was null or undefined"))
			}
			if (is.vector(marker) == FALSE) {
				return (print(paste0("Error: marker must be a vector.")))
			}
			qs$add('marker', marker, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/updates'), collapse = "")
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
				attr(contentResponse, "className") <- "PIItemsStreamUpdatesRetrieve"
			}
			return (contentResponse)
		},
		registerStreamSetUpdates = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/updates'), collapse = "")
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
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), NULL, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getValuesAdHoc = function(webId, selectedFields, sortField, sortOrder, time, timeZone, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.vector(webId) == FALSE) {
				return (print(paste0("Error: webId must be a vector.")))
			}
			qs$add('webId', webId, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/streamsets/value'), collapse = "")
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortField) == FALSE && is.null(sortField) == FALSE && sortField != "") {
				qs$add('sortField', sortField, FALSE);
				if (is.character(sortField) == FALSE) {
					return (print(paste0("Error: sortField must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(time) == FALSE && is.null(time) == FALSE && time != "") {
				qs$add('time', time, FALSE);
				if (is.character(time) == FALSE) {
					return (print(paste0("Error: time must be a string.")))
				}
			}
			if (missing(timeZone) == FALSE && is.null(timeZone) == FALSE && timeZone != "") {
				qs$add('timeZone', timeZone, FALSE);
				if (is.character(timeZone) == FALSE) {
					return (print(paste0("Error: timeZone must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsStreamValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValueAdHoc = function(values, bufferOption, updateOption) {
			qs <- customQueryString$new()
			if (is.null(values) || values == "") {
				return (paste0("Error: required parameter values was null or undefined"))
			}
			if (is.vector(values) == FALSE) {
				return (print(paste0("Error: values must be a vector.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streamsets/value'), collapse = "")
			if (missing(bufferOption) == FALSE && is.null(bufferOption) == FALSE && bufferOption != "") {
				qs$add('bufferOption', bufferOption, FALSE);
				if (is.character(bufferOption) == FALSE) {
					return (print(paste0("Error: bufferOption must be a string.")))
				}
			}
			if (missing(updateOption) == FALSE && is.null(updateOption) == FALSE && updateOption != "") {
				qs$add('updateOption', updateOption, FALSE);
				if (is.character(updateOption) == FALSE) {
					return (print(paste0("Error: updateOption must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), values, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		}
	)
)
