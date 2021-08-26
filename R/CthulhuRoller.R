library(R6)


#' An abstract R6 class representing rolls with a 
#' die of a specified number of sides.
#' @description
#' A roller has a number of die sides and may support modify rolls.
CthulhuRoller <- R6Class(
  "CthulhuRoller",
  active = list(
    
    #' @field DieSides Sides of the die
    DieSides = function(value) {
      if (missing(value)) {
        return(private$dieSides)
      } else {
        stop("Die sides cannot be changed after the die has been set.")
      }
    },
    
    #' @field ModsAllowed Does this roller support modify rolls (TRUE/FALSE)?
    ModsAllowed = function(value) {
      if (missing(value)) {
        return(private$modsAllowed)
      } else {
          stop("Mod roles cannot be activated after the die has been set.")
      }
    },
    
    #' @field ModDieSides The number of sides of a modifier die (readonly)
    ModDieSides = function(value) {
      if (missing(value)) {
        return(private$modDieSides)
      } else {
        stop("Modified die sides cannot be changed after the die has been set.")
      }
    },
    
    #' @field IsSkillRoller Does the roller satisfy the characterstics 
    #' of a skill roll (i.e. 1d100 with modify rolls allowed).
    #' (readonly)
    IsSkillRoller = function() {
      return(private$dieSides == 100 && private$modsAllowed && ModDieSides == 10)
    }
  ),
  private = list(
    #' @description dieSides Private property 
    dieSides = NULL,
    #' @description modsAllowed Private property 
    modsAllowed = FALSE, # allows modification rolls
    #' @description modDieSides Private property 
    modDieSides = NULL
    #' #' @description 
    #' logger = NULL
    
  ),
  public = list(
    #' @field ModifyType Enumeration that defines the type of a modifier.
    #' The `Bonus` is an enhancement; the `Malus` is a risk.
    ModifyType = c(Bonus = +1, None = 0, Malus = -1),
    
    #' @field Label A string that characterices the roller and may be
    #' used as button label.
    Label = NULL, # Button label
    
    
    #' @description Constructor
    #' @param dieSides Dies sides
    #' @param label A label that could qualify as button label
    #' to initiate this roller.
    #' @param modDieSides Sides of a modifier die.
    #' @return `invisible(self)`
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
      return(invisible(self))
    },
    
    # SetLogger = function(logObject) {
    #   if (is.null(logObject)) {
    #     private$logger = NULL
    #     return()
    #   }
    #   if (!isTruthy(logObject) || !R6::is.R6(logObject)) 
    #     stop("Module needs a valid logger")
    #   if (!("RollLogger" %in% class(logObject)))
    #     stop("Given class is not a roll logger")
    #   
    #   private$logger <- logObject
    #   
    #   return(invisible(self))
    # },
    
    #' @description Roll the roller to get a die roll result.
    #' @return The roll (value between 1 and the number 
    #' of sides of this roller die).
    #' @examples
    #' \dontrun{
    #' roller = CthulhuRoller(100, "W100", 10)
    #' print(roller$Roll())
    #' }
    Roll = function() {
      Result <- sample.int(private$dieSides, 1)
      
      # if (!is.null(private$logger)) 
      #   private$logger$Log(paste0("Roll: ", self$Label, " -> ", Result))
      
      return(Result)
    },

    #' @description AddModifier
    #' @param Roll The current roll that shall be modified.
    #' @param Modifier A value of enumeration `CthulhuRoller$ModifyType`.
    #' @return The modified roll (value between 1 and the number 
    #' of sides of this roller die).
    #' @examples
    #' \dontrun{
    #' roller <- CthulhuRoller$new(100, "W100", 10)
    #' print(roller$AddModifier(12, roller$ModifyType["Bonus"])) 
    #' }
    AddModifier = function(Roll, Modifier) {
      if(Roll < 1 || Roll > private$dieSides)
        stop(paste("Roll is out of range of this roller", self$Label))
      if (!private$modsAllowed || Modifier == self$ModifyType["None"])
        return(Roll)
      
      OldRoll <- Roll
      
      ModRolls <- Roll %% private$modDieSides
      if (ModRolls == 0) ModRolls <- private$modDieSides
      Roll <- Roll - ModRolls

      ModRolls <- c(ModRolls, sample.int(private$modDieSides, 1))
      Modifier <- ifelse(Modifier > 0, max(ModRolls), min(ModRolls))
      Roll <- Roll + Modifier
      
      # if (!is.null(private$logger))
      #   if(OldRoll == Roll)
      #     private$logger$Log(sprintf("Modify roll - no change", self$Label, Modifier, Roll))
      #   else
      #     private$logger$Log(sprintf("Modify roll %s by % d => %d", self$Label, Modifier, Roll))
      
      return(Roll)
    },

    #' @description MaxHardSuccess
    #' Determine the smallest target value required so that a
    #' roll would be a hard success.
    #' @param value A roll result
    #' @return Smallest target value required for a hard success
    MaxHardSuccess = function(value) {
      min(value * 2, dieSides-1)
    },
    #' @description MaxExtremeSuccess
    #' Determine the smallest target value required so that a
    #' roll would be an extreme success.
    #' @param value A roll result
    #' @return Smallest target value required for an extreme success
    MaxExtremeSuccess = function(value) {
      min(value * 5, dieSides-1)
    },
    #' @description What is the value for a critical failure?
    #' @param value A roll result
    #' @return The value characterizing a critical failure.
    MaxBotch = function(value) {
      return(ifelse(value < 50, 96, 100)) #TODO ???????????
    }
  )
)

