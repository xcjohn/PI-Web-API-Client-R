deleteHttpRequest <- function(url, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- DELETE(url, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), verbose())
        return(res)
    }
    else {
        res <- DELETE(url, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL))
        return(res)
    }
}


getHttpRequest <- function(url, queryParameters, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- GET(url, query = queryParameters, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), verbose())
        return(res)
    }
    else {
        res <- GET(url, query = queryParameters, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL))
        return(res)
    }
}

postHttpRequest <- function(url, bodyRequest, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- POST(url, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json", verbose())
        return(res)
    }
    else {
        res <- POST(url, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json")
        return(res)
    }
}

patchHttpRequest <- function(url, bodyRequest, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- PATCH(url, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json", verbose())
        return(res)
    }
    else {
        res <- PATCH(url, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json")
        return(res)
    }
}

putHttpRequest <- function(url, queryParameters, bodyRequest, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- PUT(url, query = queryParameters, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json", verbose())
        return(res)
    }
    else {
        res <- PUT(url, query = queryParameters, authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json")
        return(res)
    }
}

check.integer <- function(N) {
    res1 <- !grepl("[^[:digit:]]", format(N, digits = 20, scientific = FALSE))
    res2 <- class(N) == "numeric"
    res <- res1 && res2
    return(res)
}

generateListForQueryString <- function(vector, queryStringName) {
    queryParameters <- list()
    for (i in 1:length(vector)) {
        value = vector[i]
        key <- paste0(queryStringName, i)
        queryParameters[[key]] = value
        names(queryParameters)[i] <- queryStringName
    }
    return(queryParameters)
}

generateListForTwoQueryString <- function(vector1, queryStringName1, vector2, queryStringName2) {
    queryParameters <- list()
    for (i in 1:length(vector1)) {
        value = vector1[i]
        key <- paste0(queryStringName1, i)
        queryParameters[[key]] = value
        names(queryParameters)[i] <- queryStringName1
    }

    for (i in 1:length(vector2)) {
        value = vector2[i]
        key <- paste0(queryStringName2, i)
        queryParameters[[key]] = value
    }
    startIndex <- 1 + length(vector1)
    endIndex <- length(names(queryParameters))
    for (i in startIndex:endIndex) {
        names(queryParameters)[i] <- queryStringName2
    }
    return(queryParameters)
}

is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}