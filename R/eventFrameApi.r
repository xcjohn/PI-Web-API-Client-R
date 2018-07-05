eventFrameApi <- R6Class("eventFrameApi",
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
		getByPath = function(path, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(path) || path == "") {
				return (paste0("Error: required parameter path was null or undefined"))
			}
			if (is.character(path) == FALSE) {
				return (print(paste0("Error: path must be a string.")))
			}
			qs$add('path', path, FALSE);
			localVarPath <- paste(c(self$serviceBase, '/eventframes'), collapse = "")
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
				attr(contentResponse, "className") <- "PIEventFrame"
			}
			return (contentResponse)
		},
		get = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId), collapse = "")
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
				attr(contentResponse, "className") <- "PIEventFrame"
			}
			return (contentResponse)
		},
		update = function(webId, PIEventFrame) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIEventFrame) || PIEventFrame == "") {
				return (paste0("Error: required parameter PIEventFrame was null or undefined"))
			}
			className <- attr(PIEventFrame, "className")
			if ((is.null(className)) || (className != "PIEventFrame")) {
				return (print(paste0("Error: the class from the parameter PIEventFrame should be PIEventFrame.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId), collapse = "")
			res <- patchHttpRequest(localVarPath, qs$getQueryParameters(), PIEventFrame, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		delete = function(webId) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId), collapse = "")
			res <- deleteHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		acknowledge = function(webId) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/acknowledge'), collapse = "")
			res <- patchHttpRequest(localVarPath, qs$getQueryParameters(), NULL, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getAnnotations = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/annotations'), collapse = "")
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
				attr(contentResponse, "className") <- "PIItemsAnnotation"
			}
			return (contentResponse)
		},
		createAnnotation = function(webId, PIAnnotation, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAnnotation) || PIAnnotation == "") {
				return (paste0("Error: required parameter PIAnnotation was null or undefined"))
			}
			className <- attr(PIAnnotation, "className")
			if ((is.null(className)) || (className != "PIAnnotation")) {
				return (print(paste0("Error: the class from the parameter PIAnnotation should be PIAnnotation.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/annotations'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), PIAnnotation, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getAnnotationById = function(id, webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(id) || id == "") {
				return (paste0("Error: required parameter id was null or undefined"))
			}
			if (is.character(id) == FALSE) {
				return (print(paste0("Error: id must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/annotations/', id), collapse = "")
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
				attr(contentResponse, "className") <- "PIAnnotation"
			}
			return (contentResponse)
		},
		updateAnnotation = function(id, webId, PIAnnotation) {
			qs <- customQueryString$new()
			if (is.null(id) || id == "") {
				return (paste0("Error: required parameter id was null or undefined"))
			}
			if (is.character(id) == FALSE) {
				return (print(paste0("Error: id must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAnnotation) || PIAnnotation == "") {
				return (paste0("Error: required parameter PIAnnotation was null or undefined"))
			}
			className <- attr(PIAnnotation, "className")
			if ((is.null(className)) || (className != "PIAnnotation")) {
				return (print(paste0("Error: the class from the parameter PIAnnotation should be PIAnnotation.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/annotations/', id), collapse = "")
			res <- patchHttpRequest(localVarPath, qs$getQueryParameters(), PIAnnotation, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		deleteAnnotation = function(id, webId) {
			qs <- customQueryString$new()
			if (is.null(id) || id == "") {
				return (paste0("Error: required parameter id was null or undefined"))
			}
			if (is.character(id) == FALSE) {
				return (print(paste0("Error: id must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/annotations/', id), collapse = "")
			res <- deleteHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		deleteAnnotationAttachmentMediaById = function(id, webId) {
			qs <- customQueryString$new()
			if (is.null(id) || id == "") {
				return (paste0("Error: required parameter id was null or undefined"))
			}
			if (is.character(id) == FALSE) {
				return (print(paste0("Error: id must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/annotations/', id, '/attachment/media'), collapse = "")
			res <- deleteHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getAnnotationAttachmentMediaMetadataById = function(id, webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(id) || id == "") {
				return (paste0("Error: required parameter id was null or undefined"))
			}
			if (is.character(id) == FALSE) {
				return (print(paste0("Error: id must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/annotations/', id, '/attachment/media/metadata'), collapse = "")
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
				attr(contentResponse, "className") <- "PIMediaMetadata"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 404) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getAttributes = function(webId, categoryName, maxCount, nameFilter, searchFullHierarchy, selectedFields, showExcluded, showHidden, sortField, sortOrder, startIndex, templateName, trait, traitCategory, valueType, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/attributes'), collapse = "")
			if (missing(categoryName) == FALSE && is.null(categoryName) == FALSE && categoryName != "") {
				qs$add('categoryName', categoryName, FALSE);
				if (is.character(categoryName) == FALSE) {
					return (print(paste0("Error: categoryName must be a string.")))
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
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				qs$add('startIndex', startIndex, FALSE);
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			if (missing(templateName) == FALSE && is.null(templateName) == FALSE && templateName != "") {
				qs$add('templateName', templateName, FALSE);
				if (is.character(templateName) == FALSE) {
					return (print(paste0("Error: templateName must be a string.")))
				}
			}
			if (missing(trait) == FALSE && is.null(trait) == FALSE && trait != "") {
				qs$add('trait', trait, TRUE);
				if (is.vector(trait) == FALSE) {
					return (print(paste0("Error: trait must be a vector.")))
				}
			}
			if (missing(traitCategory) == FALSE && is.null(traitCategory) == FALSE && traitCategory != "") {
				qs$add('traitCategory', traitCategory, TRUE);
				if (is.vector(traitCategory) == FALSE) {
					return (print(paste0("Error: traitCategory must be a vector.")))
				}
			}
			if (missing(valueType) == FALSE && is.null(valueType) == FALSE && valueType != "") {
				qs$add('valueType', valueType, FALSE);
				if (is.character(valueType) == FALSE) {
					return (print(paste0("Error: valueType must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsAttribute"
			}
			return (contentResponse)
		},
		createAttribute = function(webId, PIAttribute, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIAttribute) || PIAttribute == "") {
				return (paste0("Error: required parameter PIAttribute was null or undefined"))
			}
			className <- attr(PIAttribute, "className")
			if ((is.null(className)) || (className != "PIAttribute")) {
				return (print(paste0("Error: the class from the parameter PIAttribute should be PIAttribute.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/attributes'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), PIAttribute, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		captureValues = function(webId) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/attributes/capture'), collapse = "")
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), NULL, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getCategories = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/categories'), collapse = "")
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
				attr(contentResponse, "className") <- "PIItemsElementCategory"
			}
			return (contentResponse)
		},
		createConfig = function(webId, includeChildElements) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/config'), collapse = "")
			if (missing(includeChildElements) == FALSE && is.null(includeChildElements) == FALSE && includeChildElements != "") {
				qs$add('includeChildElements', includeChildElements, FALSE);
				if (is.logical(includeChildElements) == FALSE) {
					return (print(paste0("Error: includeChildElements must be a boolean.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), NULL, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		findEventFrameAttributes = function(webId, attributeCategory, attributeDescriptionFilter, attributeNameFilter, attributeType, endTime, eventFrameCategory, eventFrameDescriptionFilter, eventFrameNameFilter, eventFrameTemplate, maxCount, referencedElementNameFilter, searchFullHierarchy, searchMode, selectedFields, sortField, sortOrder, startIndex, startTime, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/eventframeattributes'), collapse = "")
			if (missing(attributeCategory) == FALSE && is.null(attributeCategory) == FALSE && attributeCategory != "") {
				qs$add('attributeCategory', attributeCategory, FALSE);
				if (is.character(attributeCategory) == FALSE) {
					return (print(paste0("Error: attributeCategory must be a string.")))
				}
			}
			if (missing(attributeDescriptionFilter) == FALSE && is.null(attributeDescriptionFilter) == FALSE && attributeDescriptionFilter != "") {
				qs$add('attributeDescriptionFilter', attributeDescriptionFilter, FALSE);
				if (is.character(attributeDescriptionFilter) == FALSE) {
					return (print(paste0("Error: attributeDescriptionFilter must be a string.")))
				}
			}
			if (missing(attributeNameFilter) == FALSE && is.null(attributeNameFilter) == FALSE && attributeNameFilter != "") {
				qs$add('attributeNameFilter', attributeNameFilter, FALSE);
				if (is.character(attributeNameFilter) == FALSE) {
					return (print(paste0("Error: attributeNameFilter must be a string.")))
				}
			}
			if (missing(attributeType) == FALSE && is.null(attributeType) == FALSE && attributeType != "") {
				qs$add('attributeType', attributeType, FALSE);
				if (is.character(attributeType) == FALSE) {
					return (print(paste0("Error: attributeType must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(eventFrameCategory) == FALSE && is.null(eventFrameCategory) == FALSE && eventFrameCategory != "") {
				qs$add('eventFrameCategory', eventFrameCategory, FALSE);
				if (is.character(eventFrameCategory) == FALSE) {
					return (print(paste0("Error: eventFrameCategory must be a string.")))
				}
			}
			if (missing(eventFrameDescriptionFilter) == FALSE && is.null(eventFrameDescriptionFilter) == FALSE && eventFrameDescriptionFilter != "") {
				qs$add('eventFrameDescriptionFilter', eventFrameDescriptionFilter, FALSE);
				if (is.character(eventFrameDescriptionFilter) == FALSE) {
					return (print(paste0("Error: eventFrameDescriptionFilter must be a string.")))
				}
			}
			if (missing(eventFrameNameFilter) == FALSE && is.null(eventFrameNameFilter) == FALSE && eventFrameNameFilter != "") {
				qs$add('eventFrameNameFilter', eventFrameNameFilter, FALSE);
				if (is.character(eventFrameNameFilter) == FALSE) {
					return (print(paste0("Error: eventFrameNameFilter must be a string.")))
				}
			}
			if (missing(eventFrameTemplate) == FALSE && is.null(eventFrameTemplate) == FALSE && eventFrameTemplate != "") {
				qs$add('eventFrameTemplate', eventFrameTemplate, FALSE);
				if (is.character(eventFrameTemplate) == FALSE) {
					return (print(paste0("Error: eventFrameTemplate must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				qs$add('maxCount', maxCount, FALSE);
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(referencedElementNameFilter) == FALSE && is.null(referencedElementNameFilter) == FALSE && referencedElementNameFilter != "") {
				qs$add('referencedElementNameFilter', referencedElementNameFilter, FALSE);
				if (is.character(referencedElementNameFilter) == FALSE) {
					return (print(paste0("Error: referencedElementNameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(searchMode) == FALSE && is.null(searchMode) == FALSE && searchMode != "") {
				qs$add('searchMode', searchMode, FALSE);
				if (is.character(searchMode) == FALSE) {
					return (print(paste0("Error: searchMode must be a string.")))
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
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				qs$add('startIndex', startIndex, FALSE);
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsAttribute"
			}
			return (contentResponse)
		},
		getEventFrames = function(webId, canBeAcknowledged, categoryName, endTime, isAcknowledged, maxCount, nameFilter, referencedElementNameFilter, referencedElementTemplateName, searchFullHierarchy, searchMode, selectedFields, severity, sortField, sortOrder, startIndex, startTime, templateName, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/eventframes'), collapse = "")
			if (missing(canBeAcknowledged) == FALSE && is.null(canBeAcknowledged) == FALSE && canBeAcknowledged != "") {
				qs$add('canBeAcknowledged', canBeAcknowledged, FALSE);
				if (is.logical(canBeAcknowledged) == FALSE) {
					return (print(paste0("Error: canBeAcknowledged must be a boolean.")))
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
			if (missing(isAcknowledged) == FALSE && is.null(isAcknowledged) == FALSE && isAcknowledged != "") {
				qs$add('isAcknowledged', isAcknowledged, FALSE);
				if (is.logical(isAcknowledged) == FALSE) {
					return (print(paste0("Error: isAcknowledged must be a boolean.")))
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
			if (missing(referencedElementNameFilter) == FALSE && is.null(referencedElementNameFilter) == FALSE && referencedElementNameFilter != "") {
				qs$add('referencedElementNameFilter', referencedElementNameFilter, FALSE);
				if (is.character(referencedElementNameFilter) == FALSE) {
					return (print(paste0("Error: referencedElementNameFilter must be a string.")))
				}
			}
			if (missing(referencedElementTemplateName) == FALSE && is.null(referencedElementTemplateName) == FALSE && referencedElementTemplateName != "") {
				qs$add('referencedElementTemplateName', referencedElementTemplateName, FALSE);
				if (is.character(referencedElementTemplateName) == FALSE) {
					return (print(paste0("Error: referencedElementTemplateName must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(searchMode) == FALSE && is.null(searchMode) == FALSE && searchMode != "") {
				qs$add('searchMode', searchMode, FALSE);
				if (is.character(searchMode) == FALSE) {
					return (print(paste0("Error: searchMode must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(severity) == FALSE && is.null(severity) == FALSE && severity != "") {
				qs$add('severity', severity, TRUE);
				if (is.vector(severity) == FALSE) {
					return (print(paste0("Error: severity must be a vector.")))
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
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				qs$add('startIndex', startIndex, FALSE);
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
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
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsEventFrame"
			}
			return (contentResponse)
		},
		createEventFrame = function(webId, PIEventFrame, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PIEventFrame) || PIEventFrame == "") {
				return (paste0("Error: required parameter PIEventFrame was null or undefined"))
			}
			className <- attr(PIEventFrame, "className")
			if ((is.null(className)) || (className != "PIEventFrame")) {
				return (print(paste0("Error: the class from the parameter PIEventFrame should be PIEventFrame.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/eventframes'), collapse = "")
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), PIEventFrame, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getReferencedElements = function(webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/referencedelements'), collapse = "")
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
				attr(contentResponse, "className") <- "PIItemsElement"
			}
			return (contentResponse)
		},
		getSecurity = function(webId, userIdentity, forceRefresh, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(userIdentity) || userIdentity == "") {
				return (paste0("Error: required parameter userIdentity was null or undefined"))
			}
			if (is.vector(userIdentity) == FALSE) {
				return (print(paste0("Error: userIdentity must be a vector.")))
			}
			qs$add('userIdentity', userIdentity, TRUE);
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/security'), collapse = "")
			if (missing(forceRefresh) == FALSE && is.null(forceRefresh) == FALSE && forceRefresh != "") {
				qs$add('forceRefresh', forceRefresh, FALSE);
				if (is.logical(forceRefresh) == FALSE) {
					return (print(paste0("Error: forceRefresh must be a boolean.")))
				}
			}
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
				attr(contentResponse, "className") <- "PIItemsSecurityRights"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 401) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			if (res$status == 502) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getSecurityEntries = function(webId, nameFilter, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/securityentries'), collapse = "")
			if (missing(nameFilter) == FALSE && is.null(nameFilter) == FALSE && nameFilter != "") {
				qs$add('nameFilter', nameFilter, FALSE);
				if (is.character(nameFilter) == FALSE) {
					return (print(paste0("Error: nameFilter must be a string.")))
				}
			}
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
				attr(contentResponse, "className") <- "PIItemsSecurityEntry"
			}
			return (contentResponse)
		},
		createSecurityEntry = function(webId, PISecurityEntry, applyToChildren, webIdType) {
			qs <- customQueryString$new()
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PISecurityEntry) || PISecurityEntry == "") {
				return (paste0("Error: required parameter PISecurityEntry was null or undefined"))
			}
			className <- attr(PISecurityEntry, "className")
			if ((is.null(className)) || (className != "PISecurityEntry")) {
				return (print(paste0("Error: the class from the parameter PISecurityEntry should be PISecurityEntry.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/securityentries'), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				qs$add('applyToChildren', applyToChildren, FALSE);
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			if (missing(webIdType) == FALSE && is.null(webIdType) == FALSE && webIdType != "") {
				qs$add('webIdType', webIdType, FALSE);
				if (is.character(webIdType) == FALSE) {
					return (print(paste0("Error: webIdType must be a string.")))
				}
			}
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), PISecurityEntry, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getSecurityEntryByName = function(name, webId, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(name) || name == "") {
				return (paste0("Error: required parameter name was null or undefined"))
			}
			if (is.character(name) == FALSE) {
				return (print(paste0("Error: name must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/securityentries/', name), collapse = "")
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
				attr(contentResponse, "className") <- "PISecurityEntry"
			}
			if (res$status == 404) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		updateSecurityEntry = function(name, webId, PISecurityEntry, applyToChildren) {
			qs <- customQueryString$new()
			if (is.null(name) || name == "") {
				return (paste0("Error: required parameter name was null or undefined"))
			}
			if (is.character(name) == FALSE) {
				return (print(paste0("Error: name must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			if (is.null(PISecurityEntry) || PISecurityEntry == "") {
				return (paste0("Error: required parameter PISecurityEntry was null or undefined"))
			}
			className <- attr(PISecurityEntry, "className")
			if ((is.null(className)) || (className != "PISecurityEntry")) {
				return (print(paste0("Error: the class from the parameter PISecurityEntry should be PISecurityEntry.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/securityentries/', name), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				qs$add('applyToChildren', applyToChildren, FALSE);
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			res <- putHttpRequest(localVarPath, qs$getQueryParameters(), PISecurityEntry, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		deleteSecurityEntry = function(name, webId, applyToChildren) {
			qs <- customQueryString$new()
			if (is.null(name) || name == "") {
				return (paste0("Error: required parameter name was null or undefined"))
			}
			if (is.character(name) == FALSE) {
				return (print(paste0("Error: name must be a string.")))
			}
			if (is.null(webId) || webId == "") {
				return (paste0("Error: required parameter webId was null or undefined"))
			}
			if (is.character(webId) == FALSE) {
				return (print(paste0("Error: webId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/', webId, '/securityentries/', name), collapse = "")
			if (missing(applyToChildren) == FALSE && is.null(applyToChildren) == FALSE && applyToChildren != "") {
				qs$add('applyToChildren', applyToChildren, FALSE);
				if (is.logical(applyToChildren) == FALSE) {
					return (print(paste0("Error: applyToChildren must be a boolean.")))
				}
			}
			res <- deleteHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		getMultiple = function(asParallel, includeMode, path, selectedFields, webId, webIdType) {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/eventframes/multiple'), collapse = "")
			if (missing(asParallel) == FALSE && is.null(asParallel) == FALSE && asParallel != "") {
				qs$add('asParallel', asParallel, FALSE);
				if (is.logical(asParallel) == FALSE) {
					return (print(paste0("Error: asParallel must be a boolean.")))
				}
			}
			if (missing(includeMode) == FALSE && is.null(includeMode) == FALSE && includeMode != "") {
				qs$add('includeMode', includeMode, FALSE);
				if (is.character(includeMode) == FALSE) {
					return (print(paste0("Error: includeMode must be a string.")))
				}
			}
			if (missing(path) == FALSE && is.null(path) == FALSE && path != "") {
				qs$add('path', path, TRUE);
				if (is.vector(path) == FALSE) {
					return (print(paste0("Error: path must be a vector.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(webId) == FALSE && is.null(webId) == FALSE && webId != "") {
				qs$add('webId', webId, TRUE);
				if (is.vector(webId) == FALSE) {
					return (print(paste0("Error: webId must be a vector.")))
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
				attr(contentResponse, "className") <- "PIItemsItemEventFrame"
			}
			if (res$status == 207) {
				attr(contentResponse, "className") <- "PIItemsItemEventFrame"
			}
			return (contentResponse)
		},
		getEventFramesQuery = function(databaseWebId, maxCount, query, selectedFields, startIndex, webIdType) {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/eventframes/search'), collapse = "")
			if (missing(databaseWebId) == FALSE && is.null(databaseWebId) == FALSE && databaseWebId != "") {
				qs$add('databaseWebId', databaseWebId, FALSE);
				if (is.character(databaseWebId) == FALSE) {
					return (print(paste0("Error: databaseWebId must be a string.")))
				}
			}
			if (missing(maxCount) == FALSE && is.null(maxCount) == FALSE && maxCount != "") {
				qs$add('maxCount', maxCount, FALSE);
				if (check.integer(maxCount) == FALSE) {
					return (print(paste0("Error: maxCount must be an integer.")))
				}
			}
			if (missing(query) == FALSE && is.null(query) == FALSE && query != "") {
				qs$add('query', query, FALSE);
				if (is.character(query) == FALSE) {
					return (print(paste0("Error: query must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				qs$add('startIndex', startIndex, FALSE);
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
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
				attr(contentResponse, "className") <- "PIItemsEventFrame"
			}
			return (contentResponse)
		},
		createSearchByAttribute = function(PISearchByAttribute, noResults, selectedFields, webIdType) {
			qs <- customQueryString$new()
			if (is.null(PISearchByAttribute) || PISearchByAttribute == "") {
				return (paste0("Error: required parameter PISearchByAttribute was null or undefined"))
			}
			className <- attr(PISearchByAttribute, "className")
			if ((is.null(className)) || (className != "PISearchByAttribute")) {
				return (print(paste0("Error: the class from the parameter PISearchByAttribute should be PISearchByAttribute.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/searchbyattribute'), collapse = "")
			if (missing(noResults) == FALSE && is.null(noResults) == FALSE && noResults != "") {
				qs$add('noResults', noResults, FALSE);
				if (is.logical(noResults) == FALSE) {
					return (print(paste0("Error: noResults must be a boolean.")))
				}
			}
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
			res <- postHttpRequest(localVarPath, qs$getQueryParameters(), PISearchByAttribute, self$username, self$password, self$authType, self$validateSSL, self$debug)
			return (res)
		},
		executeSearchByAttribute = function(searchId, canBeAcknowledged, endTime, isAcknowledged, maxCount, nameFilter, referencedElementNameFilter, searchFullHierarchy, searchMode, selectedFields, severity, sortField, sortOrder, startIndex, startTime, webIdType) {
			qs <- customQueryString$new()
			if (is.null(searchId) || searchId == "") {
				return (paste0("Error: required parameter searchId was null or undefined"))
			}
			if (is.character(searchId) == FALSE) {
				return (print(paste0("Error: searchId must be a string.")))
			}
			localVarPath <- paste(c(self$serviceBase, '/eventframes/searchbyattribute/', searchId), collapse = "")
			if (missing(canBeAcknowledged) == FALSE && is.null(canBeAcknowledged) == FALSE && canBeAcknowledged != "") {
				qs$add('canBeAcknowledged', canBeAcknowledged, FALSE);
				if (is.logical(canBeAcknowledged) == FALSE) {
					return (print(paste0("Error: canBeAcknowledged must be a boolean.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(isAcknowledged) == FALSE && is.null(isAcknowledged) == FALSE && isAcknowledged != "") {
				qs$add('isAcknowledged', isAcknowledged, FALSE);
				if (is.logical(isAcknowledged) == FALSE) {
					return (print(paste0("Error: isAcknowledged must be a boolean.")))
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
			if (missing(referencedElementNameFilter) == FALSE && is.null(referencedElementNameFilter) == FALSE && referencedElementNameFilter != "") {
				qs$add('referencedElementNameFilter', referencedElementNameFilter, FALSE);
				if (is.character(referencedElementNameFilter) == FALSE) {
					return (print(paste0("Error: referencedElementNameFilter must be a string.")))
				}
			}
			if (missing(searchFullHierarchy) == FALSE && is.null(searchFullHierarchy) == FALSE && searchFullHierarchy != "") {
				qs$add('searchFullHierarchy', searchFullHierarchy, FALSE);
				if (is.logical(searchFullHierarchy) == FALSE) {
					return (print(paste0("Error: searchFullHierarchy must be a boolean.")))
				}
			}
			if (missing(searchMode) == FALSE && is.null(searchMode) == FALSE && searchMode != "") {
				qs$add('searchMode', searchMode, FALSE);
				if (is.character(searchMode) == FALSE) {
					return (print(paste0("Error: searchMode must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(severity) == FALSE && is.null(severity) == FALSE && severity != "") {
				qs$add('severity', severity, TRUE);
				if (is.vector(severity) == FALSE) {
					return (print(paste0("Error: severity must be a vector.")))
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
			if (missing(startIndex) == FALSE && is.null(startIndex) == FALSE && startIndex != "") {
				qs$add('startIndex', startIndex, FALSE);
				if (check.integer(startIndex) == FALSE) {
					return (print(paste0("Error: startIndex must be an integer.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
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
				attr(contentResponse, "className") <- "PIItemsEventFrame"
			}
			if (res$status == 400) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		}
	)
)
