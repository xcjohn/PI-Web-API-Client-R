'library(piwebapi)'



'TODO: The PI Web API client must provide a user name and password when using “basic” authentication'
'Store passwords outside of the code in a hardware TPM, trusted service (credential manager) or in a protected file.'
'Code to return the user name and password is not shown here.'
piWebApiService <- piwebapi$new("https://devdata.osisoft.com/piwebapi/", FALSE, "webapiuser", "!try3.14webapi!", FALSE, FALSE)
response1 = piWebApiService$home$get()


paths = c("pi:\\\\PISRV1\\sinusoid", "pi:\\\\PISRV1\\sinusoidu", "pi:\\\\PISRV1\\cdt158", "af:\\\\PISRV1\\Universities\\UC Davis\\Buildings\\Academic Surge Building\\Electricity|Demand")


response2 = piWebApiService$dataServer$getByPath("\\\\PISRV1", "WebId")



response3a = piWebApiService$point$getByPath("\\\\PISRV1\\sinusoidu")
response3b = piWebApiService$point$getByPath("\\\\PISRV1\\cdt158")
response3c = piWebApiService$point$getByPath("\\\\PISRV1\\sinusoid")

webIds <- c(response3a$WebId, response3b$WebId, response3c$WebId)
response4 = piWebApiService$streamSet$getValuesAdHoc(webIds)


response5a <- piWebApiService$data$getRecordedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-200d", endTime = "t")
response5b <- piWebApiService$data$getRecordedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-200d", endTime = "t", selectedFields = "items.value;items.timestamp")
response5c <- piWebApiService$data$getRecordedValues(path = "af:\\\\PISRV1\\Universities\\UC Davis\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-200d", endTime = "t")

response6a <- piWebApiService$data$getInterpolatedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-20d", endTime = "t-19d", interval = "1h")
response6b <- piWebApiService$data$getInterpolatedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", interval = "1h", selectedFields = "items.value;items.timestamp")
response6c <- piWebApiService$data$getInterpolatedValues(path = "af:\\\\PISRV1\\Universities\\UC Davis\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-2d", endTime = "t", interval = "1h")


response7a <- piWebApiService$data$getPlotValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", intervals = 30)
response7b <- piWebApiService$data$getPlotValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", intervals = 30, selectedFields = "items.value;items.timestamp")
response7c <- piWebApiService$data$getPlotValues(path = "af:\\\\PISRV1\\UCDavisBuildings\\Buildings\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-2d", endTime = "t", intervals = 30)


response8a <- piWebApiService$data$getMultipleRecordedValues(paths = paths, startTime = "y-200d", endTime = "t-195d")
response9a <- piWebApiService$data$getMultipleInterpolatedValues(paths = paths, startTime = "y-200d", endTime = "t-195d", interval = "1h")
response10a <- piWebApiService$data$getMultiplePlotValues(paths = paths, startTime ="y-200d", endTime = "t-195d", intervals = 30)


response8b <- piWebApiService$data$getMultipleRecordedValues(paths = paths, startTime = "y-200d", endTime = "t", selectedFields = "items.items.value;items.items.timestamp")
response9b <- piWebApiService$data$getMultipleInterpolatedValues(paths = paths, startTime = "y-200d", endTime = "t", interval = "1h", selectedFields = "items.items.value;items.items.timestamp")
response10b <- piWebApiService$data$getMultiplePlotValues(paths = paths, startTime = "y-200d", endTime = "t", intervals = 30, selectedFields = "items.items.value;items.items.timestamp")

response11 <- piWebApiService$calculation$getAtTimes("tagval('sinusoid','*-1d')", NULL, NULL, c("y", "t"), response2$WebId)


response12 <- piWebApiService$element$getByPath("\\\\PISRV1\\City Bikes\\(TO)BIKE")
response13 <- piWebApiService$elementTemplate$getByPath("\\\\PISRV1\\City Bikes\\ElementTemplates[BikeStationTemplate]")
valueQueries = list();
valUeQuery1 <- PIValueQuery(attributeName = "Latitude", attributeValue = 0, searchOperator = "GreaterThan")
valueQueries[[1]] = valUeQuery1
search1 <- PISearchByAttribute(searchRoot = response12$WebId, elementTemplate = response13$WebId,  valueQueries = valueQueries);

response14 <-piWebApiService$element$createSearchByAttribute(search1, FALSE, NULL)
searchId = substr(response14$headers$location,65,1000000)

response15 <-piWebApiService$element$executeSearchByAttribute(searchId)




