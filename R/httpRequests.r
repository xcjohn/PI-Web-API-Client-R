deleteHttpRequest <- function(url, queryParameters, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- DELETE(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), verbose())
		showError(res)
		return(res)
    }
    else {
        res <- DELETE(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL))
		showError(res)
		return(res)
    }
}


getHttpRequest <- function(url, queryParameters, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- GET(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), verbose())
		showError(res)
		return(res)
    }
    else {
        res <- GET(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL))
		showError(res)
		return(res)
    }
}

postHttpRequest <- function(url, queryParameters, bodyRequest, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- POST(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json", verbose())
		showError(res)
		return(res)
    }
    else {
        res <- POST(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json")
		showError(res)
		return(res)
    }
}

patchHttpRequest <- function(url, queryParameters, bodyRequest, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- PATCH(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json", verbose())
		showError(res)
		return(res)
    }
    else {
        res <- PATCH(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json")
		showError(res)
		return(res)
    }
}

putHttpRequest <- function(url, queryParameters, bodyRequest, username, password, authType, validateSSL, debug) {
    if (debug == TRUE) {
        res <- PUT(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json", verbose())
		    showError(res)
		    return(res)
    }
    else {
        res <- PUT(url, query = queryParameters, add_headers('X-Requested-With' = 'PIWebApiWrapper'), authenticate(user = username, password = password, type = authType), config = httr::config(ssl_verifypeer = validateSSL), body = bodyRequest, encode = "json")
        showError(res)
		    return(res)
    }
}

showError <- function(res) {

    if (res$status > 299) {
        print(paste0("HTTP Status code is ", res$status, "."))
        if (res$status == 401) {
          print(paste0("Authentication Error: Please review PI Web API security and allowed authentication methods."))
        }
		    error <- content(res)
		    if (is.null(error$Errors[[1]])==FALSE)
		    {
		        print(paste0("Error: ", error$Errors[[1]]))
		    }
		    else
	      {
	          print(paste0("Error: ", error))
	      }
	  }
}

check.integer <- function(N) {
    res1 <- !grepl("[^[:digit:]]", format(N, digits = 20, scientific = FALSE))
    res2 <- class(N) == "numeric"
    res <- res1 && res2
    return(res)
}


is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}
