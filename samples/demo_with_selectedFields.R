
piWebApiService <- piwebapi$new("https://devdata.osisoft.com/piwebapi/", FALSE, "webapiuser", "password", FALSE, FALSE)
response1 = piWebApiService$home$get()


paths = c("pi:\\\\PISRV1\\sinusoid", "pi:\\\\PISRV1\\sinusoidu", "pi:\\\\PISRV1\\cdt158", "af:\\\\PISRV1\\UCDavisBuildings\\Buildings\\Buildings\\Academic Surge Building\\Electricity|Demand")


response2 = piWebApiService$dataServer$getByPath("\\\\PISRV1", "WebId")


response5a <- piWebApiService$data$getRecordedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-200d", endTime = "t")
response5b <- piWebApiService$data$getRecordedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-200d", endTime = "t", selectedFields = "items.value;items.timestamp")
response5c <- piWebApiService$data$getRecordedValues(path = "af:\\\\PISRV1\\UCDavisBuildings\\Buildings\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-200d", endTime = "t")

response6a <- piWebApiService$data$getInterpolatedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", interval = "1h")
response6b <- piWebApiService$data$getInterpolatedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", interval = "1h", selectedFields = "items.value;items.timestamp")
response6c <- piWebApiService$data$getInterpolatedValues(path = "af:\\\\PISRV1\\UCDavisBuildings\\Buildings\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-2d", endTime = "t", interval = "1h")


response7a <- piWebApiService$data$getPlotValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", intervals = 30)
response7b <- piWebApiService$data$getPlotValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", intervals = 30, selectedFields = "items.value;items.timestamp")
response7c <- piWebApiService$data$getPlotValues(path = "af:\\\\PISRV1\\UCDavisBuildings\\Buildings\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-2d", endTime = "t", intervals = 30)


response3a = piWebApiService$point$getByPath("\\\\PISRV1\\sinusoidu")
response3b = piWebApiService$point$getByPath("\\\\PISRV1\\cdt158")
response3c = piWebApiService$point$getByPath("\\\\PISRV1\\sinusoid")

webIds <- c(response3a$WebId, response3b$WebId, response3c$WebId)
response4 = piWebApiService$streamSet$getValuesAdHoc(webIds)



response8a <- piWebApiService$data$getMultipleRecordedValues(paths = paths, startTime = "y-200d", endTime = "t")
response9a <- piWebApiService$data$getMultipleInterpolatedValues(paths = paths, startTime = "y-200d", endTime = "t", interval = "1h")
response10a <- piWebApiService$data$getMultiplePlotValues(paths = paths, startTime = "y-200d", endTime = "t", intervals = 30)


response8b <- piWebApiService$data$getMultipleRecordedValues(paths = paths, startTime = "y-200d", endTime = "t", selectedFields = "items.items.value;items.items.timestamp")
response9b <- piWebApiService$data$getMultipleInterpolatedValues(paths = paths, startTime = "y-200d", endTime = "t", interval = "1h", selectedFields = "items.items.value;items.items.timestamp")
response10b <- piWebApiService$data$getMultiplePlotValues(paths = paths, startTime = "y-200d", endTime = "t", intervals = 30, selectedFields = "items.items.value;items.items.timestamp")

