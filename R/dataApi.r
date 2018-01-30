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
                       stop("Error: invalid path. It needs to start with \"pi\" or \"af\"")
                     },
                     convertPathsToWebIds = function(paths) {
                       lengthPaths <- length(paths)
                       webIds <- array(1:lengthPaths)
                       for (count in 1:lengthPaths) {
                         webIds[count] <- self$convertPathToWebId(paths[count])
                       }
                       return(as.vector(webIds))
                     },
                     convertToDataFrame = function(items, selectedFields) {

                       if (is.null(items) == TRUE)
                         stop("The returned data is Null")

                       streamsLength <- length(items)
                       if (streamsLength == 0)
                         stop('The returned data is Null')

                       addValues = FALSE
                       addTimeStamp = FALSE
                       addUnitAbbr = FALSE
                       addGood = FALSE
                       addQuestionable = FALSE
                       addSubstituded = FALSE

                       if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
                         if (grepl("timestamp", selectedFields)) {
                           addTimeStamp = TRUE
                         }
                         if (grepl("value", selectedFields)) {
                           addValues = TRUE
                         }
                         if (grepl("questionable", selectedFields)) {
                           addQuestionable = TRUE
                         }
                         if (grepl("unitabbr", selectedFields)) {
                           addUnitAbbr = TRUE
                         }
                         if (grepl("good", selectedFields)) {
                           addGood = TRUE
                         }
                         if (grepl("substituted", selectedFields)) {
                           addSubstituded = TRUE
                         }
                       }
                       else {
                         addValues = TRUE
                         addTimeStamp = TRUE
                         addUnitAbbr = TRUE
                         addGood = TRUE
                         addQuestionable = TRUE
                         addSubstituded = TRUE
                       }

                       itemsLength <- length(items)
                       if (itemsLength == 0) {
                         value <- array(0)
                         timestamp <- array(0)
                         unitsAbbreviation <- array(0)
                         good <- array(0)
                         questionable <- array(0)
                         substituted <- array(0)
                         resDataFrame <- data.frame(value, timestamp, unitsAbbreviation, good, questionable, substituted)
                         return(resDataFrame)
                       }

                       value = NULL
                       unitsAbbreviation = NULL
                       timestamp = NULL
                       good = NULL
                       questionable = NULL
                       substituted = NULL

                       if (addValues == TRUE) {
                         value <- array(1:itemsLength)
                       }
                       if (addTimeStamp == TRUE) {
                         timestamp <- array(1:itemsLength)
                       }
                       if (addUnitAbbr == TRUE) {
                         unitsAbbreviation <- array(1:itemsLength)
                       }
                       if (addGood == TRUE) {
                         good <- array(1:itemsLength)
                       }
                       if (addQuestionable == TRUE) {
                         questionable <- array(1:itemsLength)
                       }
                       if (addSubstituded == TRUE) {
                         substituted <- array(1:itemsLength)
                       }
                       for (i in 1:itemsLength) {
                         if (addValues == TRUE) {
                           if (is.numeric(items[[i]]$Value) == TRUE) {
                             value[i] <- items[[i]]$Value
                           }
                           else {
                             value[i] <- items[[i]]$Value$Name
                           }
                         }
                         if (addTimeStamp == TRUE) {
                           timestamp[i] <- items[[i]]$Timestamp
                         }
                         if (addQuestionable == TRUE) {
                           questionable[i] <- items[[i]]$Questionable
                         }
                         if (addUnitAbbr == TRUE) {
                           unitsAbbreviation[i] <- items[[i]]$UnitsAbbreviation
                         }
                         if (addGood == TRUE) {
                           good[i] <- items[[i]]$Good
                         }
                         if (addSubstituded == TRUE) {
                           substituted[i] <- items[[i]]$Substituted
                         }
                       }

                       index = 1:itemsLength

                       resDataFrame <- data.frame(index)
                       if (addTimeStamp == TRUE) {
                         resDataFrame[["timestamp"]] = as.vector(timestamp)
                       }
                       if (addValues == TRUE) {
                         resDataFrame[["value"]] = as.vector(value)
                       }
                       if (addUnitAbbr == TRUE) {
                         resDataFrame[["unitsAbbreviation"]] = as.vector(unitsAbbreviation)
                       }
                       if (addGood == TRUE) {
                         resDataFrame[["good"]] = as.vector(good)
                       }
                       if (addQuestionable == TRUE) {
                         resDataFrame[["questionable"]] = as.vector(questionable)
                       }
                       if (addSubstituded == TRUE) {
                         resDataFrame[["substituted"]] = as.vector(substituted)
                       }

                       resDataFrame$index <- NULL
                       head(resDataFrame)
                       return(resDataFrame)

                     },
                     calculateItemsIndex = function(webId, items) {
                       for (i in 1:length(items)) {
                         if (items[[i]]$WebId == webId) {
                           return(i)
                         }
                       }
                       return(-1)
                     },
                     convertMultipleStreamsToDataFrame = function(items, gatherInOneDataFrame, webIds, selectedFields, paths = NULL) {

                       if (is.null(items) == TRUE)
                         stop("The returned data is Null")

                       streamsLength <- length(items)
                       if (streamsLength == 0)
                         stop('The returned data is Null')


                       for (i in 1:streamsLength) {
                         if ((is.null(items[i]) == TRUE) || (is.null(items[[i]]$Items) == TRUE))
                           stop('Some items are Null')
                       }


                       if (gatherInOneDataFrame == TRUE) {

                         addValues = FALSE
                         addTimeStamp = FALSE
                         addUnitAbbr = FALSE
                         addGood = FALSE
                         addQuestionable = FALSE
                         addSubstituded = FALSE

                         if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
                           if (grepl("timestamp", selectedFields)) {
                             addTimeStamp = TRUE
                           }
                           if (grepl("value", selectedFields)) {
                             addValues = TRUE
                           }
                           if (grepl("questionable", selectedFields)) {
                             addQuestionable = TRUE
                           }
                           if (grepl("unitabbr", selectedFields)) {
                             addUnitAbbr = TRUE
                           }
                           if (grepl("good", selectedFields)) {
                             addGood = TRUE
                           }
                           if (grepl("substituted", selectedFields)) {
                             addSubstituded = TRUE
                           }
                         }
                         else {
                           addValues = TRUE
                           addTimeStamp = TRUE
                           addUnitAbbr = TRUE
                           addGood = TRUE
                           addQuestionable = TRUE
                           addSubstituded = TRUE
                         }

                         itemsLength <- length(items[[1]]$Items)
                         index = 1:itemsLength
                         resDataFrame <- data.frame(index)
                         for (i in 1:streamsLength) {
                           k = self$calculateItemsIndex(webIds[i], items);
                           if (addValues == TRUE) {
                             value <- array(1:itemsLength)
                           }
                           if (addTimeStamp == TRUE) {
                             timestamp <- array(1:itemsLength)
                           }
                           if (addUnitAbbr == TRUE) {
                             unitsAbbreviation <- array(1:itemsLength)
                           }
                           if (addGood == TRUE) {
                             good <- array(1:itemsLength)
                           }
                           if (addQuestionable == TRUE) {
                             questionable <- array(1:itemsLength)
                           }
                           if (addSubstituded == TRUE) {
                             substituted <- array(1:itemsLength)
                           }
                           for (j in 1:itemsLength) {
                             if (addTimeStamp == TRUE) {
                               timestamp[j] <- items[[k]]$Items[[j]]$Timestamp
                             }
                             if (addUnitAbbr == TRUE) {
                               unitsAbbreviation[j] <- items[[k]]$Items[[j]]$UnitsAbbreviation
                             }
                             if (addGood == TRUE) {
                               good[j] <- items[[i]]$Items[[k]]$Good
                             }
                             if (addQuestionable == TRUE) {
                               questionable[j] <- items[[k]]$Items[[j]]$Questionable
                             }
                             if (addSubstituded == TRUE) {
                               substituted[j] <- items[[k]]$Items[[j]]$Substituted
                             }
                             if (addValues == TRUE) {
                               if (is.numeric(items[[k]]$Items[[j]]$Value) == TRUE) {
                                 value[j] <- items[[k]]$Items[[j]]$Value
                               }
                               else {
                                 value[j] <- items[[k]]$Items[[j]]$Value$Name
                               }
                             }
                           }


                           if (i == 1) {
                             if (addTimeStamp == TRUE) {
                               resDataFrame[["timestamp"]] = as.vector(timestamp)
                             }
                           }


                           if (addValues == TRUE) {
                             resDataFrame[[paste0("value", i)]] = as.vector(value)
                           }
                           if (addUnitAbbr == TRUE) {
                             resDataFrame[[paste0("unitsAbbreviation", i)]] = as.vector(unitsAbbreviation)
                           }
                           if (addGood == TRUE) {
                             resDataFrame[[paste0("good", i)]] = as.vector(good)
                           }
                           if (addQuestionable == TRUE) {
                             resDataFrame[[paste0("questionable", i)]] = as.vector(questionable)
                           }
                           if (addSubstituded == TRUE) {
                             resDataFrame[[paste0("substituted", i)]] = as.vector(substituted)
                           }
                         }
                         resDataFrame$index <- NULL
                         head(resDataFrame)
                       }
                       else {
                         resDataFrame <- list()
                         for (i in 1:streamsLength) {
                           key <- paste0(paths[i])
                           df <- self$convertToDataFrame(items[[i]]$Items, selectedFields)
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
                         stop("Error: path must be a string.")
                       }
                       webId <- self$convertPathToWebId(path)
                       res <- self$stream$getRecorded(webId, boundaryType, desiredUnits, endTime, filterExpression, includeFilteredValues, maxCount, selectedFields, startTime, timeZone);
                       resDataFrame <- self$convertToDataFrame(res$Items, selectedFields)
                       return(resDataFrame);
                     },
                     getMultipleRecordedValues = function(paths, boundaryType, endTime, filterExpression, includeFilteredValues, maxCount, selectedFields, sortField, sortOrder, startTime, timeZone, webIdType) {
                       if (is.null(paths) || paths == "") {
                         stop("Error: required parameter paths was null or undefined")
                       }
                       if (is.vector(paths) == FALSE) {
                         stop("Error: path must be a vector.")
                       }
                       webIds <- self$convertPathsToWebIds(paths);
                       res <- self$streamSet$getRecordedAdHoc(webIds, boundaryType, endTime, filterExpression, includeFilteredValues, maxCount, selectedFields, sortField, sortOrder, startTime, timeZone, webIdType);
                       resDataFrame <- self$convertMultipleStreamsToDataFrame(res$Items, FALSE, webIds, selectedFields, paths)
                       return(resDataFrame);
                     },
                     getInterpolatedValues = function(path, desiredUnits, endTime, filterExpression, includeFilteredValues, interval, selectedFields, startTime, syncTime, syncTimeBoundaryType, timeZone) {
                       if (is.null(path) || path == "") {
                         stop("Error: required parameter path was null or undefined")
                       }
                       if (is.character(path) == FALSE) {
                         stop("Error: path must be a string.")
                       }
                       webId <- self$convertPathToWebId(path);
                       res <- self$stream$getInterpolated(webId, desiredUnits, endTime, filterExpression, includeFilteredValues, interval, selectedFields, startTime, syncTime, syncTimeBoundaryType, timeZone);
                       resDataFrame <- self$convertToDataFrame(res$Items, selectedFields)
                       return(resDataFrame);
                     },
                     getMultipleInterpolatedValues = function(paths, endTime, filterExpression, includeFilteredValues, interval, selectedFields, sortField, sortOrder, startTime, syncTime, syncTimeBoundaryType, timeZone, webIdType) {
                       if (is.null(paths) || paths == "") {
                         return(paste0("Error: required parameter paths was null or undefined"))
                       }
                       if (is.vector(paths) == FALSE) {
                         stop("Error: path must be a vector.")
                       }
                       if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
                         if (grepl("items.webid", selectedFields) == FALSE) {
                           selectedFields = paste0(selectedFields, ";items.webid")
                         }
                       }
                       webIds <- self$convertPathsToWebIds(paths);
                       res <- self$streamSet$getInterpolatedAdHoc(webIds, endTime, filterExpression, includeFilteredValues, interval, selectedFields, sortField, sortOrder, startTime, syncTime, syncTimeBoundaryType, timeZone, webIdType);
                       resDataFrame <- self$convertMultipleStreamsToDataFrame(res$Items, TRUE, webIds, selectedFields)
                       return(resDataFrame);
                     },
                     getPlotValues = function(path, desiredUnits, endTime, intervals, selectedFields, startTime, timeZone) {
                       if (is.null(path) || path == "") {
                         return(paste0("Error: required parameter path was null or undefined"))
                       }
                       if (is.character(path) == FALSE) {
                         stop("Error: path must be a string.")
                       }
                       webId <- self$convertPathToWebId(path);
                       res <- self$stream$getPlot(webId, desiredUnits, endTime, intervals, selectedFields, startTime, timeZone);
                       resDataFrame <- self$convertToDataFrame(res$Items, selectedFields)
                       return(resDataFrame);
                     },
                     getMultiplePlotValues = function(paths, endTime, intervals, selectedFields, sortField, sortOrder, startTime, timeZone, webIdType) {
                       if (is.null(paths) || paths == "") {
                         return(paste0("Error: required parameter paths was null or undefined"))
                       }
                       if (is.vector(paths) == FALSE) {
                         stop("Error: path must be a vector.")
                       }

                       webIds <- self$convertPathsToWebIds(paths);

                       res <- self$streamSet$getPlotAdHoc(webIds, endTime, intervals, selectedFields, sortField, sortOrder, startTime, timeZone, webIdType);
                       resDataFrame <- self$convertMultipleStreamsToDataFrame(res$Items, FALSE, webIds, selectedFields, paths)
                       return(resDataFrame);
                     }
                   )
)
