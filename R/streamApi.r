streamApi <- R6Class("streamApi",
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
		getChannel = function(webId, heartbeatRate, includeInitialValues, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/channel'), collapse = "")
			if (missing(heartbeatRate) == FALSE && is.null(heartbeatRate) == FALSE && heartbeatRate != "") {
				queryParameters$heartbeatRate <- heartbeatRate
				if (check.integer(heartbeatRate) == FALSE) {
					return (print(paste0("Error: heartbeatRate must be an integer.")))
				}
			}
			if (missing(includeInitialValues) == FALSE && is.null(includeInitialValues) == FALSE && includeInitialValues != "") {
				queryParameters$includeInitialValues <- includeInitialValues
				if (is.logical(includeInitialValues) == FALSE) {
					return (print(paste0("Error: includeInitialValues must be a boolean.")))
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
			if (res$status == 101) {
				attr(contentResponse, "className") <- "PIItemsStreamValues"
			}
			return (contentResponse)
		},
		getEnd = function(webId, desiredUnits, selectedFields) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/end'), collapse = "")
			if (missing(desiredUnits) == FALSE && is.null(desiredUnits) == FALSE && desiredUnits != "") {
				queryParameters$desiredUnits <- desiredUnits
				if (is.character(desiredUnits) == FALSE) {
					return (print(paste0("Error: desiredUnits must be a string.")))
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
				attr(contentResponse, "className") <- "PITimedValue"
			}
			return (contentResponse)
		},
		getInterpolated = function(webId, desiredUnits, endTime, filterExpression, includeFilteredValues, interval, selectedFields, startTime, syncTime, syncTimeBoundaryType, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/interpolated'), collapse = "")
			if (missing(desiredUnits) == FALSE && is.null(desiredUnits) == FALSE && desiredUnits != "") {
				queryParameters$desiredUnits <- desiredUnits
				if (is.character(desiredUnits) == FALSE) {
					return (print(paste0("Error: desiredUnits must be a string.")))
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
			if (missing(syncTime) == FALSE && is.null(syncTime) == FALSE && syncTime != "") {
				queryParameters$syncTime <- syncTime
				if (is.character(syncTime) == FALSE) {
					return (print(paste0("Error: syncTime must be a string.")))
				}
			}
			if (missing(syncTimeBoundaryType) == FALSE && is.null(syncTimeBoundaryType) == FALSE && syncTimeBoundaryType != "") {
				queryParameters$syncTimeBoundaryType <- syncTimeBoundaryType
				if (is.character(syncTimeBoundaryType) == FALSE) {
					return (print(paste0("Error: syncTimeBoundaryType must be a string.")))
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
				attr(contentResponse, "className") <- "PITimedValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getInterpolatedAtTimes = function(webId, desiredUnits, filterExpression, includeFilteredValues, selectedFields, sortOrder, time, timeZone) {
			queryParameters <- generateListForQueryString(time, "time")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/interpolatedattimes'), collapse = "")
			if (missing(desiredUnits) == FALSE && is.null(desiredUnits) == FALSE && desiredUnits != "") {
				queryParameters$desiredUnits <- desiredUnits
				if (is.character(desiredUnits) == FALSE) {
					return (print(paste0("Error: desiredUnits must be a string.")))
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
			if (missing(time) == FALSE && is.null(time) == FALSE && time != "") {
				queryParameters$time <- time
				if (is.vector(time) == FALSE) {
					return (print(paste0("Error: time must be a vector.")))
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
				attr(contentResponse, "className") <- "PITimedValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getPlot = function(webId, desiredUnits, endTime, intervals, selectedFields, startTime, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/plot'), collapse = "")
			if (missing(desiredUnits) == FALSE && is.null(desiredUnits) == FALSE && desiredUnits != "") {
				queryParameters$desiredUnits <- desiredUnits
				if (is.character(desiredUnits) == FALSE) {
					return (print(paste0("Error: desiredUnits must be a string.")))
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
				attr(contentResponse, "className") <- "PITimedValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecorded = function(webId, boundaryType, desiredUnits, endTime, filterExpression, includeFilteredValues, maxCount, selectedFields, startTime, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/recorded'), collapse = "")
			if (missing(boundaryType) == FALSE && is.null(boundaryType) == FALSE && boundaryType != "") {
				queryParameters$boundaryType <- boundaryType
				if (is.character(boundaryType) == FALSE) {
					return (print(paste0("Error: boundaryType must be a string.")))
				}
			}
			if (missing(desiredUnits) == FALSE && is.null(desiredUnits) == FALSE && desiredUnits != "") {
				queryParameters$desiredUnits <- desiredUnits
				if (is.character(desiredUnits) == FALSE) {
					return (print(paste0("Error: desiredUnits must be a string.")))
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
				attr(contentResponse, "className") <- "PITimedValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
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
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/recorded'), collapse = "")
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
		getRecordedAtTime = function(webId, time, desiredUnits, retrievalMode, selectedFields, timeZone) {
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
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/recordedattime'), collapse = "")
			if (missing(desiredUnits) == FALSE && is.null(desiredUnits) == FALSE && desiredUnits != "") {
				queryParameters$desiredUnits <- desiredUnits
				if (is.character(desiredUnits) == FALSE) {
					return (print(paste0("Error: desiredUnits must be a string.")))
				}
			}
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
				attr(contentResponse, "className") <- "PITimedValue"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getRecordedAtTimes = function(webId, desiredUnits, retrievalMode, selectedFields, sortOrder, time, timeZone) {
			queryParameters <- generateListForQueryString(time, "time")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/recordedattimes'), collapse = "")
			if (missing(desiredUnits) == FALSE && is.null(desiredUnits) == FALSE && desiredUnits != "") {
				queryParameters$desiredUnits <- desiredUnits
				if (is.character(desiredUnits) == FALSE) {
					return (print(paste0("Error: desiredUnits must be a string.")))
				}
			}
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
			if (missing(time) == FALSE && is.null(time) == FALSE && time != "") {
				queryParameters$time <- time
				if (is.vector(time) == FALSE) {
					return (print(paste0("Error: time must be a vector.")))
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
				attr(contentResponse, "className") <- "PITimedValues"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getSummary = function(webId, calculationBasis, endTime, filterExpression, sampleInterval, sampleType, selectedFields, startTime, summaryDuration, summaryType, timeType, timeZone) {
			queryParameters <- generateListForQueryString(summaryType, "summaryType")
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/summary'), collapse = "")
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
				attr(contentResponse, "className") <- "PIItemsSummaryValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getValue = function(webId, desiredUnits, selectedFields, time, timeZone) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/value'), collapse = "")
			if (missing(desiredUnits) == FALSE && is.null(desiredUnits) == FALSE && desiredUnits != "") {
				queryParameters$desiredUnits <- desiredUnits
				if (is.character(desiredUnits) == FALSE) {
					return (print(paste0("Error: desiredUnits must be a string.")))
				}
			}
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
				attr(contentResponse, "className") <- "PITimedValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateValue = function(webId, PITimedValue, bufferOption, updateOption, webIdType) {
			queryParameters <- list()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PITimedValue) || PITimedValue == "") {
				return (paste0("Error: required parameter PITimedValue was null or undefined"))
			}
			className <- attr(PITimedValue, "className")
			if ((is.null(className)) || (className != "PITimedValue")) {
				return (print(paste0("Error: the class from the parameter PITimedValue should be PITimedValue.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId, '/value'), collapse = "")
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				queryParameters$webIdType <- webIdType
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, PITimedValue, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		}
	)
)
