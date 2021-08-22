library(R6)


CthulhuRoller <- R6Class(
  "CthulhuRoller",
  public = list(
    die = NULL,
    label = NULL,
    modrolls = FALSE, # allows modification rolls
    moddice = NULL,
    
    initialize = function(die = NA, label = NA, moddice = NA) {
      if (is.na(die) || !is.numeric(die)) stop("A die is missing")
      
      self$die   <- die
      self$label <- ifelse(is.character(label), label, paste0("1d", die))
      if (is.numeric(moddice) && moddice > 1) {
        self$modrolls <- TRUE
        self$moddice  <- moddice
      } else {
        self$modrolls <- FALSE
        self$moddice  <- NULL
      }
    },
    
    roll = function(Modify = FALSE) {
      Result <- sample.int(self$die, 1)
      
      if (self$modrolls && !isFALSE(Modify) && Modify != 0) {
        First  <- Result %% self$moddice
        Second <- sample.int(self$moddice, 1)
        Result <- Result - First
        Modifier <- ifelse(Modify > 0, max(First, Second), min(First, Second))
        Result <- Result + Modifier
      }
      
      return(Result)
    }

  )
)

