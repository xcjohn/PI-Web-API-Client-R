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
            itemsLength <- length(items)
            value <- array(1:itemsLength)
            timestamp <- array(1:itemsLength)
            unitsAbbreviation <- array(1:itemsLength)
            good <- array(1:itemsLength)
            questionable <- array(1:itemsLength)
            substituted <- array(1:itemsLength)
            for (i in 1:itemsLength) {
                if (is.null(items[[i]]$Value) == FALSE)
                {
                  if (is.numeric(items[[i]]$Value) == TRUE)
                  {
                    value[i] <- items[[i]]$Value
                  }
                  else
                  {
                    value[i] <- items[[i]]$Value$Name
                  }
                }
                if (is.null(items[[i]]$Timestamp) == FALSE)
                {
                  timestamp[i] <- items[[i]]$Timestamp
                }
                if (is.null(items[[i]]$UnitsAbbreviation) == FALSE)
                {
                  unitsAbbreviation[i] <- items[[i]]$UnitsAbbreviation
                }
                if (is.null(items[[i]]$Good) == FALSE)
                {
                  good[i] <- items[[i]]$Good
                }
                if (is.null(items[[i]]$Questionable) == FALSE)
                {
                  questionable[i] <- items[[i]]$Questionable
                }
                if (is.null(items[[i]]$Substituted) == FALSE)
                {
                  substituted[i] <- items[[i]]$Substituted
                }
            }


            resDataFrame <- data.frame(value, timestamp, unitsAbbreviation, good, questionable, substituted)
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
            streamsLength <- length(items)

            if (gatherInOneDataFrame == TRUE) {
                itemsLength <- length(items[[1]]$Items)
                order <- 1:itemsLength
                resDataFrame <- data.frame(order)
                for (i in 1:streamsLength) {
                    k = self$calculateItemsIndex (webIds[i], items, i);
                    value <- array(1:itemsLength)
                    timestamp <- array(1:itemsLength)
                    unitsAbbreviation <- array(1:itemsLength)
                    good <- array(1:itemsLength)
                    questionable <- array(1:itemsLength)
                    substituted <- array(1:itemsLength)
                    for (j in 1:itemsLength) {
                      if (is.null(items[[k]]$Items[[j]]$Value) == FALSE)
                      {
                        if (is.numeric(items[[k]]$Items[[j]]$Value) == TRUE)
                        {
                          value[j] <- items[[k]]$Items[[j]]$Value
                        }
                        else
                        {
                          value[j] <- items[[k]]$Items[[j]]$Value$Name
                        }
                      }
                      if (is.null(items[[k]]$Items[[j]]$Timestamp) == FALSE)
                      {
                        timestamp[j] <- items[[k]]$Items[[j]]$Timestamp
                      }
                      if (is.null(items[[k]]$Items[[j]]$UnitsAbbreviation) == FALSE)
                      {
                        unitsAbbreviation[j] <- items[[k]]$Items[[j]]$UnitsAbbreviation
                      }
                      if (is.null(items[[k]]$Items[[k]]$Good) == FALSE)
                      {
                        good[j] <- items[[k]]$Items[[k]]$Good
                      }
                      if (is.null(items[[k]]$Items[[j]]$Questionable) == FALSE)
                      {
                        questionable[j] <- items[[k]]$Items[[j]]$Questionable
                      }
                      if (is.null(items[[k]]$Items[[j]]$Substituted) == FALSE)
                      {
                        substituted[j] <- items[[k]]$Items[[j]]$Substituted
                      }
                    }
                    if (i == 1) {
                        resDataFrame <- data.frame(timestamp)
                    }

                    resDataFrame[[paste0("value", i)]] = as.vector(value)
                    resDataFrame[[paste0("unitsAbbreviation", i)]] = as.vector(unitsAbbreviation)
                    resDataFrame[[paste0("good", i)]] = as.vector(good)
                    resDataFrame[[paste0("questionable", i)]] = as.vector(questionable)
                    resDataFrame[[paste0("substituted", i)]] = as.vector(substituted)
                }
            }
            else {
                resDataFrame <- list()
                for (i in 1:streamsLength) {
                    key <- paste0(paths[i])
                    df <- self$convertToDataFrame(items[[i]]$Items)
                    resDataFrame[[key]] = df
                }
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
