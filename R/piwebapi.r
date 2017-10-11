library(R6)
library(httr)
library(rjson)
source("httpRequests.r")
source("models//PIAnalysis.r")
source("models//PIAnalysisCategory.r")
source("models//PIAnalysisRule.r")
source("models//PIAnalysisRulePlugIn.r")
source("models//PIAnalysisTemplate.r")
source("models//PIAnnotation.r")
source("models//PIAssetDatabase.r")
source("models//PIAssetServer.r")
source("models//PIAttribute.r")
source("models//PIAttributeCategory.r")
source("models//PIAttributeTemplate.r")
source("models//PIAttributeTrait.r")
source("models//PICacheInstance.r")
source("models//PIDataServer.r")
source("models//PIElement.r")
source("models//PIElementCategory.r")
source("models//PIElementTemplate.r")
source("models//PIEnumerationSet.r")
source("models//PIEnumerationValue.r")
source("models//PIErrors.r")
source("models//PIEventFrame.r")
source("models//PIItemAttribute.r")
source("models//PIItemElement.r")
source("models//PIItemEventFrame.r")
source("models//PIItemPoint.r")
source("models//PIItemsAnalysis.r")
source("models//PIItemsAnalysisCategory.r")
source("models//PIItemsAnalysisRule.r")
source("models//PIItemsAnalysisRulePlugIn.r")
source("models//PIItemsAnalysisTemplate.r")
source("models//PIItemsAnnotation.r")
source("models//PIItemsAssetDatabase.r")
source("models//PIItemsAssetServer.r")
source("models//PIItemsAttribute.r")
source("models//PIItemsAttributeCategory.r")
source("models//PIItemsAttributeTemplate.r")
source("models//PIItemsAttributeTrait.r")
source("models//PIItemsCacheInstance.r")
source("models//PIItemsDataServer.r")
source("models//PIItemsElement.r")
source("models//PIItemsElementCategory.r")
source("models//PIItemsElementTemplate.r")
source("models//PIItemsEnumerationSet.r")
source("models//PIItemsEnumerationValue.r")
source("models//PIItemsEventFrame.r")
source("models//PIItemsItemAttribute.r")
source("models//PIItemsItemElement.r")
source("models//PIItemsItemEventFrame.r")
source("models//PIItemsItemPoint.r")
source("models//PIItemsItemsSubstatus.r")
source("models//PIItemsPoint.r")
source("models//PIItemsPointAttribute.r")
source("models//PIItemsSecurityEntry.r")
source("models//PIItemsSecurityIdentity.r")
source("models//PIItemsSecurityMapping.r")
source("models//PIItemsSecurityRights.r")
source("models//PIItemsStreamSummaries.r")
source("models//PIItemsStreamValue.r")
source("models//PIItemsStreamValues.r")
source("models//PIItemsSubstatus.r")
source("models//PIItemsSummaryValue.r")
source("models//PIItemsTable.r")
source("models//PIItemsTableCategory.r")
source("models//PIItemsTimeRulePlugIn.r")
source("models//PIItemsUnitClass.r")
source("models//PILanding.r")
source("models//PIPoint.r")
source("models//PIPointAttribute.r")
source("models//PIRequest.r")
source("models//PIRequestTemplate.r")
source("models//PIResponse.r")
source("models//PISecurity.r")
source("models//PISecurityEntry.r")
source("models//PISecurityIdentity.r")
source("models//PISecurityMapping.r")
source("models//PISecurityRights.r")
source("models//PIStreamSummaries.r")
source("models//PIStreamValue.r")
source("models//PIStreamValues.r")
source("models//PISubstatus.r")
source("models//PISummaryValue.r")
source("models//PISystemLanding.r")
source("models//PISystemStatus.r")
source("models//PITable.r")
source("models//PITableCategory.r")
source("models//PITableData.r")
source("models//PITimedValue.r")
source("models//PITimedValues.r")
source("models//PITimeRule.r")
source("models//PITimeRulePlugIn.r")
source("models//PIUnit.r")
source("models//PIUnitClass.r")
source("models//PIUserInfo.r")
source("models//PIValue.r")
source("models//PIVersion.r")
source("api//dataApi.r")
source("api//homeApi.r")
source("api//analysisApi.r")
source("api//analysisCategoryApi.r")
source("api//analysisRulePlugInApi.r")
source("api//analysisRuleApi.r")
source("api//analysisTemplateApi.r")
source("api//assetDatabaseApi.r")
source("api//assetServerApi.r")
source("api//attributeCategoryApi.r")
source("api//attributeApi.r")
source("api//attributeTemplateApi.r")
source("api//attributeTraitApi.r")
source("api//batchApi.r")
source("api//calculationApi.r")
source("api//channelApi.r")
source("api//dataServerApi.r")
source("api//elementCategoryApi.r")
source("api//elementApi.r")
source("api//elementTemplateApi.r")
source("api//enumerationSetApi.r")
source("api//enumerationValueApi.r")
source("api//eventFrameApi.r")
source("api//pointApi.r")
source("api//securityIdentityApi.r")
source("api//securityMappingApi.r")
source("api//streamApi.r")
source("api//streamSetApi.r")
source("api//systemApi.r")
source("api//configurationApi.r")
source("api//tableCategoryApi.r")
source("api//tableApi.r")
source("api//timeRulePlugInApi.r")
source("api//timeRuleApi.r")
source("api//unitClassApi.r")
source("api//unitApi.r")
piwebapi <- R6Class("piwebapi",
	private = list(),
	public = list(
		serviceBase = NULL,
		authType = NULL,
		username = NULL,
		password = NULL,
		validateSSL = NULL,
		debug = NULL,
		home = NULL,
		analysis = NULL,
		analysisCategory = NULL,
		analysisRulePlugIn = NULL,
		analysisRule = NULL,
		analysisTemplate = NULL,
		assetDatabase = NULL,
		assetServer = NULL,
		attributeCategory = NULL,
		attribute = NULL,
		attributeTemplate = NULL,
		attributeTrait = NULL,
		batch = NULL,
		calculation = NULL,
		channel = NULL,
		dataServer = NULL,
		elementCategory = NULL,
		element = NULL,
		elementTemplate = NULL,
		enumerationSet = NULL,
		enumerationValue = NULL,
		eventFrame = NULL,
		point = NULL,
		securityIdentity = NULL,
		securityMapping = NULL,
		stream = NULL,
		streamSet = NULL,
		system = NULL,
		configuration = NULL,
		tableCategory = NULL,
		table = NULL,
		timeRulePlugIn = NULL,
		timeRule = NULL,
		unitClass = NULL,
		unit = NULL,
		data = NULL,
		initialize = function(baseUrl, useKerberos, username, password, validateSSL = TRUE, debug = FALSE) {
			self$serviceBase <- baseUrl
			self$validateSSL <- validateSSL
			self$debug <- debug
			if (useKerberos == FALSE) {
				self$authType <- "basic"
				self$username <- username
				self$password <- password
			}
			else {
				self$authType <- "gssnegotiate"
				self$username <- ""
				self$password <- ""
			}
			self$home = homeApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$analysis = analysisApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$analysisCategory = analysisCategoryApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$analysisRulePlugIn = analysisRulePlugInApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$analysisRule = analysisRuleApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$analysisTemplate = analysisTemplateApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$assetDatabase = assetDatabaseApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$assetServer = assetServerApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$attributeCategory = attributeCategoryApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$attribute = attributeApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$attributeTemplate = attributeTemplateApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$attributeTrait = attributeTraitApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$batch = batchApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$calculation = calculationApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$channel = channelApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$dataServer = dataServerApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$elementCategory = elementCategoryApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$element = elementApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$elementTemplate = elementTemplateApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$enumerationSet = enumerationSetApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$enumerationValue = enumerationValueApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$eventFrame = eventFrameApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$point = pointApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$securityIdentity = securityIdentityApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$securityMapping = securityMappingApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$stream = streamApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$streamSet = streamSetApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$system = systemApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$configuration = configurationApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$tableCategory = tableCategoryApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$table = tableApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$timeRulePlugIn = timeRulePlugInApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$timeRule = timeRuleApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$unitClass = unitClassApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$unit = unitApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$data = dataApi$new(self$attribute, self$point, self$stream, self$streamSet)
		}
	)
)
