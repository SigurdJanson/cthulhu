

test_that("Properties are correctly set by constructor", {
  DieList <- c(3, 4, 10, 100)
  ModDieList <- list(NA, 0, NULL, 10)
  LabelList <- paste0("Test", DieList, "Name")
  ModsAllowed <- c(FALSE, FALSE, FALSE, TRUE)
  
  for(i in 1:4) {
    roller <- CthulhuRoller$new(DieList[i], LabelList[i], ModDieList[[i]])
    
    expect_equal(roller$DieSides, DieList[i])
    expect_equal(roller$Label, LabelList[i])
    expect_equal(roller$ModsAllowed, ModsAllowed[i])
    
    if (ModsAllowed[i])
      expect_equal(roller$ModDieSides, ModDieList[[i]])
    else
      expect_null(roller$ModDieSides)
  }
})


test_that("rolls are within range", {
  TestDice <- c(3, 4, 6, 10, 20, 100)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test")
    
    for (j in 1:100) {
      obs <- roller$Roll()
      expect_gte(obs, 1)
      expect_lte(obs, TestDice[i])
    }
  }
})



test_that("un-modified bonus rolls do not change", {
  TestDice <- c(20, 100)
  Modifiers <- c(2, 10)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test", Modifiers[i])
    
    exp <- roller$Roll()
    obs <- roller$AddModifier(exp, roller$ModifyType["None"])
    expect_equal(obs, exp)
  }
})



test_that("modified bonus rolls are within range", {
  TestDice <- c(20, 100)
  Modifiers <- c(2, 10)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test", Modifiers[i])
    
    for (j in 1:TestDice[i]) {
      obs <- roller$AddModifier(j, roller$ModifyType["Bonus"])
      expect_gte(obs, 1, label = paste("Roll =", j))
      expect_lte(obs, TestDice[i], label = paste("Roll =", j))
    }
  }
})

test_that("modified malus rolls are within range", {
  TestDice <- c(20, 100)
  Modifiers <- c(2, 10)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test", Modifiers[i])
    
    for (j in 1:TestDice[i]) {
      obs <- roller$AddModifier(j, roller$ModifyType["Malus"])
      expect_gte(obs, 1, label = paste("Roll =", j))
      expect_lte(obs, TestDice[i], label = paste("Roll =", j))
    }
  }
})