dataApi <- R6Class("dataApi",
    private = list(),
    public = list(
        attribute = NULL,
        point = NULL,
        stream = NULL,
        streamSet = NULL,
        initialize = function(attribute, point, stream, streamSet) {
            self$stream = stream
            self$streamSet = streamSet
            self$point = point
            self$attribute = attribute
        },
        convertPathToWebId = function(fullPath) {
            system <- substr(fullPath, 1, 3)
            path <- substr(fullPath, 4, 100000)
            if (system == "af:") {
                res <- self$attribute$getByPath(path)
                return(res$WebId)
            }
            if (system == "pi:") {
                res <- self$point$getByPath(path)
                return(res$WebId)
            }
            return(print(paste0("Error: invalid path. It needs to start with \"pi\" or \"af\"")))
        },
        convertPathsToWebIds = function(paths) {
            lengthPaths <- length(paths)
            webIds <- array(1:lengthPaths)
            for (count in 1:lengthPaths) {
                webIds[count] <- self$convertPathToWebId(paths[count])
            }
            return(as.vector(webIds))
        },
        convertToDataFrame = function(items) {
            resDataFrame <- data.table::rbindlist(items, fill = TRUE)[, lapply(.SD, unlist, use.names = FALSE)]
            data.table::setnames(resDataFrame,
                c("timestamp", "value", "unitsAbbreviation", "good", "questionable", "substituted", "annotated")
            )
            data.table::set(resDataFrame, NULL, "annotated", NULL)
            data.table::setcolorder(resDataFrame, "value")
            return(resDataFrame)
        },
        calculateItemsIndex = function(webId, items, originalIndex){
            for (i in 1:length(items)) {
              if (is.null(items[[i]]$WebId) == FALSE)
              {
                if (items[[i]]$WebId == webId) {
                  return(i)
                }
              }
            }
            return(originalIndex)
        },
        convertMultipleStreamsToDataFrame = function(items, gatherInOneDataFrame, webIds, paths = NULL) {
            resDataFrame <- lapply(items, function (item) self$convertToDataFrame(item$Items))
            data.table::setattr(resDataFrame, "names", paths)

            if (gatherInOneDataFrame == TRUE) {
                timestamp <- resDataFrame[[1]][, .SD, .SDcols = "timestamp"]

                for (i in seq_along(resDataFrame)) {
                    data.table::setnames(resDataFrame[[i]], NULL, "timestamp", NULL)
                    data.table::setnames(resDataFrame[[i]], paste0(names(resDataFrame[[1]]), "i"))
                }

                resDataFrame <- Reduce(cbind, c(list(timestamp), resDataFrame))

            }

            return(resDataFrame)
        },


        getRecordedValues = function(path, boundaryType, desiredUnits, endTime, filterExpression, includeFilteredValues, maxCount, selectedFields, startTime, timeZone) {
            if (is.null(path) || path == "") {
                return(paste0("Error: required parameter path was null or undefined"))
            }
            if (is.character(path) == FALSE) {
                return(print(paste0("Error: path must be a string.")))
            }
            webId <- self$convertPathToWebId(path)
            res <- self$stream$getRecorded(webId=webId, boundaryType=boundaryType, desiredUnits=desiredUnits, endTime=endTime, filterExpression=filterExpression, includeFilteredValues=includeFilteredValues,  maxCount=maxCount, selectedFields=selectedFields, startTime=startTime, timeZone=timeZone);
            resDataFrame <- self$convertToDataFrame(res$Items)
            return(resDataFrame);
        },
        getMultipleRecordedValues = function(paths, boundaryType, endTime, filterExpression, includeFilteredValues, maxCount, selectedFields, startTime, timeZone) {
            if (is.null(paths) || paths == "") {
                return(paste0("Error: required parameter paths was null or undefined"))
            }
            if (is.vector(paths) == FALSE) {
                return(print(paste0("Error: path must be a vector.")))
            }
            webIds <- self$convertPathsToWebIds(paths);
            res <- self$streamSet$getRecordedAdHoc(webId=webIds, boundaryType=boundaryType, endTime=endTime, filterExpression=filterExpression, includeFilteredValues=includeFilteredValues, maxCount=maxCount, selectedFields=selectedFields, startTime=startTime, timeZone=timeZone);
            resDataFrame <- self$convertMultipleStreamsToDataFrame(res$Items, FALSE, webIds, paths)
            return(resDataFrame);
        },
        getInterpolatedValues = function(path, desiredUnits, endTime, filterExpression, includeFilteredValues, interval, selectedFields, startTime, timeZone) {
            if (is.null(path) || path == "") {
                return(paste0("Error: required parameter path was null or undefined"))
            }
            if (is.character(path) == FALSE) {
                return(print(paste0("Error: path must be a string.")))
            }
            webId <- self$convertPathToWebId(path);
            res <- self$stream$getInterpolated(webId=webId, desiredUnits=desiredUnits, endTime=endTime, filterExpression=filterExpression, includeFilteredValues=includeFilteredValues, interval=interval, selectedFields=selectedFields, startTime=startTime, timeZone=timeZone);
            resDataFrame <- self$convertToDataFrame(res$Items)
            return(resDataFrame);
        },
        getMultipleInterpolatedValues = function(paths, endTime, filterExpression, includeFilteredValues, interval, selectedFields, startTime, timeZone) {
            if (is.null(paths) || paths == "") {
                return(paste0("Error: required parameter paths was null or undefined"))
            }
            if (is.vector(paths) == FALSE) {
                return(print(paste0("Error: path must be a vector.")))
            }
            webIds <- self$convertPathsToWebIds(paths);
            res <- self$streamSet$getInterpolatedAdHoc(webId=webIds, endTime=endTime, filterExpression=filterExpression, includeFilteredValues=includeFilteredValues, interval=interval, selectedFields=selectedFields, startTime=startTime, timeZone=timeZone);
            resDataFrame <- self$convertMultipleStreamsToDataFrame(res$Items, TRUE, webIds)
            return(resDataFrame);
        },
        getPlotValues = function(path, desiredUnits, endTime, intervals, selectedFields, startTime, timeZone) {
            if (is.null(path) || path == "") {
                return(paste0("Error: required parameter path was null or undefined"))
            }
            if (is.character(path) == FALSE) {
                return(print(paste0("Error: path must be a string.")))
            }
            webId <- self$convertPathToWebId(path);
            res <- self$stream$getPlot(webId=webId, desiredUnits=desiredUnits, endTime=endTime, intervals=intervals, selectedFields=selectedFields, startTime=startTime, timeZone=timeZone);
            resDataFrame <- self$convertToDataFrame(res$Items)
            return(resDataFrame);
        },
        getMultiplePlotValues = function(paths, endTime, intervals, selectedFields, startTime, timeZone) {
            if (is.null(paths) || paths == "") {
                return(paste0("Error: required parameter paths was null or undefined"))
            }
            if (is.vector(paths) == FALSE) {
                return(print(paste0("Error: path must be a vector.")))
            }

            webIds <- self$convertPathsToWebIds(paths);

            res <- self$streamSet$getPlotAdHoc(webId=webIds, endTime=endTime, intervals=intervals, selectedFields=selectedFields, startTime=startTime, timeZone=timeZone);
            resDataFrame <- self$convertMultipleStreamsToDataFrame(res$Items, FALSE, webIds, paths)
            return(resDataFrame);
        }
    )
)
