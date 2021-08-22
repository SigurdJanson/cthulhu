library(R6)


CthulhuRoller <- R6Class(
  "CthulhuRoller",
  active = list(
    DieSides = function(value) {
      if (missing(value)) {
        return(private$dieSides)
      } else {
        stop("Die sides cannot be changed after the die has been set.")
      }
    },
    
    ModsAllowed = function(value) {
      if (missing(value)) {
        return(private$modsAllowed)
      } else {
          stop("Mod roles cannot be activated after the die has been set.")
      }
    },
    
    ModDieSides = function(value) {
      if (missing(value)) {
        return(private$modDieSides)
      } else {
        stop("Modified die sides cannot be changed after the die has been set.")
      }
    }
  ),
  private = list(
    dieSides = NULL,
    modsAllowed = FALSE, # allows modification rolls
    modDieSides = NULL
  ),
  public = list(

    Label = NULL, # Button label
    
    initialize = function(dieSides = NA, label = NA, modDieSides = NA) {
      if (is.na(dieSides) || !is.numeric(dieSides)) 
        stop("Number of die sides is missing")
      
      if (dieSides > 1)
        private$dieSides <- dieSides
      else
        stop("Number of die sides has to be > 1")
      
      self$Label <- ifelse(is.character(label), label, paste0("1d", dieSides))
      if (is.numeric(modDieSides) && modDieSides > 1) {
        private$modsAllowed <- TRUE
        private$modDieSides <- modDieSides
      } else {
        private$modsAllowed <- FALSE
        private$modDieSides  <- NULL
      }
    },
    
    Roll = function(Modify = FALSE) {
      Result <- sample.int(private$dieSides, 1)
      
      if (private$modsAllowed && !isFALSE(Modify) && Modify != 0) {
        First  <- Result %% private$modDieSides
        Second <- sample.int(private$modDieSides, 1)
        Result <- Result - First
        Modifier <- ifelse(Modify > 0, max(First, Second), min(First, Second))
        Result <- Result + Modifier
      }
      
      return(Result)
    }

  )
)

