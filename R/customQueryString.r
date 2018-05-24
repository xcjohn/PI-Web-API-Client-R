

customQueryString <- R6Class("customQueryString",
	private = list(),
	public = list(
	  qsValue = list(),
		initialize = function() {
		},
		getQueryParameters = function() {
		  return(self$qsValue)
		},
		add = function(parameterKey, parameterValue, isMulti) {
		  if (isMulti == TRUE) {
		    for (i in 1:length(parameterValue)) {
		      val = parameterValue[i];
		      key = paste(parameterKey,i, sep = "");
		      self$qsValue[[key]] = val
		    }

		    for (i in 1:length(parameterValue)) {
		      key = paste(parameterKey,i, sep = "");
		      for (j in 1:length(names(self$qsValue))) {
		        if (names(self$qsValue)[j] == key)
		        {
		          names(self$qsValue)[j] <- parameterKey
		        }
		      }
		    }
		  }
		  else
		  {
		    self$qsValue[[parameterKey]] = parameterValue
		  }
		}
	)
)
