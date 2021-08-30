library(R6)


#' An abstract R6 class providing means to log strings in a 
#' list of entries.
RollLogger <- R6Class(
  "RollLogger",
  active = list( ##
    
    #' @field Length Current length of the log
    Length = function(value) {
      if (missing(value))
        return(length(private$log))
      else
        stop("Length cannot be set")
    }
  ),
  private = list( ##
    #' @description log The logged list
    log = NULL, # list of logged entries
    #' @description maxEntries Maximum allowed entries
    maxEntries = NULL
  ),
  public = list( ##
    #' @description Constructor
    #' @param MaxEntries Maximum allowed entries; default is 25
    #' @return `invisible(self)`
    initialize = function(MaxEntries = 25) {
      private$log = list()
      private$maxEntries <- MaxEntries
      return(invisible(self))
    },
    
    
    #' @description Stores strings in the log.
    #' @param ToLog a string that represents the last roll or any other event.
    #' @return  `invisible(self)`
    Log = function(ToLog) {
      # 
      if (!is.character(ToLog)) stop("Invalid logging string")
      
      if (length(private$log) == private$maxEntries)
        private$log <- private$log[-1]
      
      private$log <- c(private$log, ToLog)
      return(invisible(self))
    },
    
    
    #' @description Returns the log content as HTML string
    #' @param Reverse if `TRUE` the recent entries are shown on top.
    #' @return HTML output
    AsHtml = function(Reverse = TRUE) {
      if (Reverse)
        paste0(rev(private$log), collapse="<br/>")
      else
        paste0(private$log, collapse="<br/>")
    },
    
    
    #' @description Prints the log
    #' @param ... Arguments passed over to `cat()`
    #' @return `invisible(self)`
    print = function(...) {
      cat(paste0(private$log, collapse="\n"), ...)
      return(invisible(self))
      
    }
  )
)