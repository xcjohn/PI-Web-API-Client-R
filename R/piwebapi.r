library(R6)
library(httr)
library(rjson)


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
		notificationContactTemplate = NULL,
		notificationRule = NULL,
		notificationRuleSubscriber = NULL,
		notificationRuleTemplate = NULL,
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
			self$notificationContactTemplate = notificationContactTemplateApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$notificationRule = notificationRuleApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$notificationRuleSubscriber = notificationRuleSubscriberApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
			self$notificationRuleTemplate = notificationRuleTemplateApi$new(self$serviceBase, self$authType, self$username, self$password, self$validateSSL, self$debug)
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
