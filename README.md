PI Web API client R package (2018)
=========

## Introduction

This is an R package that integrates the PI System with R through PI Web API. With this package, you can retrieve PI data without having to generate the URL for each request. This version was developed on top of the PI Web API 2018 swagger specification.

## Requirements.

 - PI Web API 2018 installed within your domain using Kerberos or Basic Authentication. If you are using an older version, some methods might not work.
 - R 3.4.3+

## Installation

This R package is not available on CRAN. You should download it directly from this GitHub repository by using the devtools R package. If you don't have it installed, please use the command below:

```r
install.packages("devtools")
```

Then, load the library and install the PI Web API R package with the install_github method: 

```r
library(devtools)
install_github("osimloeff/PI-Web-API-Client-R")
```

If the installation is successful, the command below will load the package:

```r
library(piwebapi)
```

If you want to uninstall this package, use the command below:

```r
remove.packages("piwebapi")
```

## Documentation

All the methods and classes from this R package are described on its documentation, which can be opened by typing on the R console:

```r
help(package="piwebapi") 
```

## Notes

 - Is is highly recommended to turn debug mode on in case you are using PI Web API 2017 R2+ in order to receive more detailed exception errors. This can be achieved by creating or editing the DebugMode attribute's value to TRUE from the System Configuration element.
 - The X-Requested-With header is added to work with CSRF defences.

## Examples

Please refer to the following examples to understand how to use this library: 


### Create an intance of the piwebapi top level object.

#### Basic Authentication
```r
useKerberos <- FALSE
username <- "myusername"
password <- "mypassword"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://webserver/piwebapi", useKerberos, username, password, validateSSL, debug)
```

#### Kerberos Authentication
```r
useKerberos <- TRUE
username <- NULL
password <- NULL
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://webserver/piwebapi", useKerberos, username, password, validateSSL, debug)
```

If you want to use basic authentication instead of Kerberos, set useKerberos to FALSE.
If you are having issues with your SSL certificate and you want to ignore this error, set validateSSL to FALSE.
If you want to receive a log about each HTTP request, set debug to TRUE.

### Retrieve data from the main PI Web API endpoint

```r
response1 = piWebApiService$home$get()
```

### Get the PI Data Archive WebId

```r
response2 = piWebApiService$dataServer$getByPath("\\\\piservername", "WebId")
```


### Get current values in bulk using the StreamSet/GetValuesAdHoc

```r
response3a = piWebApiService$point$getByPath("\\\\JUPITER001\\sinusoidu")
response3b = piWebApiService$point$getByPath("\\\\JUPITER001\\cdt158")
response3c = piWebApiService$point$getByPath("\\\\JUPITER001\\sinusoid")
webIds <- c(response3a$WebId, response3b$WebId, response3c$WebId)
response3d = piWebApiService$streamSet$getValuesAdHoc(webIds)
```

### Retrieving PI data to an R data frame


```r
response4a <- piWebApiService$data$getRecordedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-200d", endTime = "t")
response4b <- piWebApiService$data$getRecordedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-200d", endTime = "t", selectedFields = "items.value;items.timestamp")
response4c <- piWebApiService$data$getRecordedValues(path = "af:\\\\PISRV1\\UCDavisBuildings\\Buildings\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-200d", endTime = "t")

response5a <- piWebApiService$data$getInterpolatedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", interval = "1h")
response5b <- piWebApiService$data$getInterpolatedValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", interval = "1h", selectedFields = "items.value;items.timestamp")
response5c <- piWebApiService$data$getInterpolatedValues(path = "af:\\\\PISRV1\\UCDavisBuildings\\Buildings\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-2d", endTime = "t", interval = "1h")

response6a <- piWebApiService$data$getPlotValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", intervals = 30)
response6b <- piWebApiService$data$getPlotValues(path = "pi:\\\\PISRV1\\sinusoid", startTime = "y-2d", endTime = "t", intervals = 30, selectedFields = "items.value;items.timestamp")
response6c <- piWebApiService$data$getPlotValues(path = "af:\\\\PISRV1\\UCDavisBuildings\\Buildings\\Buildings\\Academic Surge Building\\Electricity|Demand", startTime = "y-2d", endTime = "t", intervals = 30)

response7a <- piWebApiService$data$getMultipleRecordedValues(paths = paths, startTime = "y-200d", endTime = "t")
response7b <- piWebApiService$data$getMultipleRecordedValues(paths = paths, startTime = "y-200d", endTime = "t", selectedFields = "items.items.value;items.items.timestamp")

response8a <- piWebApiService$data$getMultipleInterpolatedValues(paths = paths, startTime = "y-200d", endTime = "t", interval = "1h")
response8b <- piWebApiService$data$getMultipleInterpolatedValues(paths = paths, startTime = "y-200d", endTime = "t", interval = "1h", selectedFields = "items.items.value;items.items.timestamp")

response9a <- piWebApiService$data$getMultiplePlotValues(paths = paths, startTime = "y-200d", endTime = "t", intervals = 30)
response9b <- piWebApiService$data$getMultiplePlotValues(paths = paths, startTime = "y-200d", endTime = "t", intervals = 30, selectedFields = "items.items.value;items.items.timestamp")
```

The path from the methods above should start with "pi:" (if your stream is a PI Point) or "af:" (if your stream is an AF attribute).



### Create a PI Point

```r
newPoint <- PIPoint(NULL, NULL, "SINUSOIDR", NULL, "12 Hour Sine Wave", "classic", "Float32", NULL, NULL, NULL, NULL, NULL)
response10 = piWebApiService$dataServer$createPoint("s0TJVKOA0Ws0KihcA8rM1GogUElGSVRORVNTLVNSVjI", newPoint)
```


### Send values in bulk using the StreamSet/UpdateValuesAdHoc

```r
timedValue1 <- PITimedValue(timestamp = "2017-04-26T17:40:54Z", value = 30)
timedValue2 <- PITimedValue(timestamp = "2017-04-27T17:40:54Z", value = 31)
timedValue3 <- PITimedValue(timestamp = "2017-04-26T17:40:54Z", value = 32)
timedValue4 <- PITimedValue(timestamp = "2017-04-27T17:40:54Z", value = 33)
t1 <- list(timedValue1, timedValue2)
t2 <- list(timedValue3, timedValue4)
s1 <- PIStreamValues(webId = webIds[1], items = t1);
s2 <- PIStreamValues(webId = webIds[2], items = t2);
values <- list(s1, s2)
response11 <- piWebApiService$streamSet$updateValuesAdHoc(values, "BufferIfPossible", "Replace");
```

### Update the description from a PI Point

```r
createdPoint <- piWebApiService$point$getByPath("\\\\PIFITNESS-SRV2\\SINUSOIDR")
updatePoint <- PIPoint()
updatePoint$Descriptor <- "12 Hour Sine Wave for R"
response12 <- piWebApiService$point$update(createdPoint$WebId, updatePoint)
```

### Delete a PI Point

```r
response13 <- piWebApiService$point$delete(createdPoint$WebId)
```

### Using PI Batch to increase performance

```r
getSinReq <- list(Method = "GET", Resource = "https://cross-platform-lab-uc2017.osisoft.com/piwebapi/points?path=\\\\pifitness-srv2\\sinusoid")
getCdtReq <- list(Method = "GET", Resource = "https://cross-platform-lab-uc2017.osisoft.com/piwebapi/points?path=\\\\pifitness-srv2\\cdt158")
getData <- list(Method = "GET", Resource = "https://cross-platform-lab-uc2017.osisoft.com/piwebapi/streamsets/value?webid={0}&webid={1}")
getData$Parameters <- c("$.sinu.Content.WebId", "$.cdt.Content.WebId")
getData$ParentIds <- c("sinu", "cdt")
batch <- list(sinu = getSinReq, cdt = getCdtReq, data = getData);
response14 <- piWebApiService$batch$execute(batch)
content(response11)
```

### Create a SecurityEntry on an element
```r
allowRight <- array(1:2)
allowRight[1] = "Read"
allowRight[2] = "ReadData"
denyRights <- array(1:3)
denyRights[1] = "Write"
denyRights[2] = "Execute"
denyRights[3] = "Admin"

securityEntry <- PISecurityEntry(securityIdentityName = "SwaggerIdentity", allowRights = as.list(allowRight), denyRights = as.list(denyRights))
response15 <- piWebApiService$element$createSecurityEntry(elementWebId, securityEntry, TRUE);
```

### Get a SecurityEntry of an element

```r
response16 <- piWebApiService$element$getSecurityEntries(elementWebId)
```

### Update a SecurityEntry of an element

```r
allowRight <- array(1)
allowRight[1] = "Read"
denyRights <- array(1:4)
denyRights[1] = "Write"
denyRights[2] = "Execute"
denyRights[3] = "Admin"
denyRights[4] = "ReadData"
securityEntry <- PISecurityEntry(allowRights = allowRight, denyRights = denyRights)
response17 <- piWebApiService$element$updateSecurityEntry("SwaggerIdentity", elementWebId, securityEntry, TRUE)
```

### PISearchByAttribute examples

```r
response12 <- piWebApiService$element$getByPath("\\\\PISRV1\\City Bikes\\(TO)BIKE")
response13 <- piWebApiService$elementTemplate$getByPath("\\\\PISRV1\\City Bikes\\ElementTemplates[BikeStationTemplate]")
valueQueries = list();
valUeQuery1 <- PIValueQuery(attributeName = "Latitude", attributeValue = 0, searchOperator = "GreaterThan")
valueQueries[[1]] = valUeQuery1
search1 <- PISearchByAttribute(searchRoot = response12$WebId, elementTemplate = response13$WebId,  valueQueries = valueQueries);

response14 <-piWebApiService$element$createSearchByAttribute(search1, FALSE, NULL)
searchId = substr(response14$headers$location,65,1000000)

response15 <-piWebApiService$element$executeSearchByAttribute(searchId)

```

### StreamUpdates

```r
response16 <- piWebApiService$streamSet$registerStreamSetUpdates(webIds);
piItemsStreamUpdatesRegister <- content(response14)
markers <- c(piItemsStreamUpdatesRegister$Items[[1]]$LatestMarker, piItemsStreamUpdatesRegister$Items[[2]]$LatestMarker, piItemsStreamUpdatesRegister$Items[[3]]$LatestMarker)
response17 <- piWebApiService$streamSet$retrieveStreamSetUpdates(markers);
```


## Licensing
Copyright 2018 OSIsoft, LLC.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
   
Please see the file named [LICENSE.md](LICENSE.md).
