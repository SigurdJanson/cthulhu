library(R6)


#' An abstract R6 class representing rolls with a 
#' die of a specified number of sides.
#' @description
#' A roller has a number of die sides and may support modify rolls.
RollLogger <- R6Class(
  "RollLogger",
  active = list( ##
    Length = function(value) {
      if (!is.null(value)) stop("Length cannot be set")
      return(length(log))
    }
  ),
  private = list( ##
    log = NULL, # list of logged entries
    maxEntries = NULL
  ),
  public = list( ##
    #' @description Constructor
    #' @param dieSides Dies sides
    #' @param label A label that could qualify as button label
    #' to initiate this roller.
    #' @param modDieSides Sides of a modifier die.
    #' @return `invisible(self)`
    initialize = function(MaxEntries = 25) {
      private$log = list()
      private$maxEntries <- MaxEntries
    },
    
    Log = function(ToLog) {
      # ToLog is a string that represents 
      # the last roll
      if (!is.character(ToLog)) stop("Invalid logging string")
      
      if (length(private$log) == private$maxEntries)
        private$log <- private$log[-1]
      
      private$log <- c(private$log, ToLog)
      return(invisible(self))
    },
    
    AsHtml = function(Reverse = TRUE) {
      if (Reverse)
        paste0(rev(private$log), collapse="<br/>")
      else
        paste0(private$log, collapse="<br/>")
    },
    
    print = function(...) {
      cat(paste0(private$log, collapse="\n"))
      invisible(self)
    }
  )
)