library(R6)


#' An abstract R6 class representing rolls with a 
#' die of a specified number of sides.
#' @description
#' A roller has a number of die sides and may support modify rolls.
RollLogger <- R6Class(
  "RollLogger",
  active = list( ##
  ),
  private = list( ##
    log = NULL # list of logged entries
  ),
  public = list( ##
    #NotifyStateChange = NULL,
    #' @description Constructor
    #' @param dieSides Dies sides
    #' @param label A label that could qualify as button label
    #' to initiate this roller.
    #' @param modDieSides Sides of a modifier die.
    #' @return `invisible(self)`
    initialize = function() {
      private$log = list()
    },
    
    Log = function(ToLog) {
      # ToLog is a string provided by a CthulhuRoller class that represents the last roll
      if (!is.character(ToLog)) stop("Invalid logging string")
        
      private$log <- c(private$log, ToLog)
      return(invisible(self))
    },
    
    AsHtml = function() {
      paste0(private$log, collapse="<br/>")
    },
    
    print = function(...) {
      cat(paste0(private$log, collapse="\n"))
      invisible(self)
    }
  )
)