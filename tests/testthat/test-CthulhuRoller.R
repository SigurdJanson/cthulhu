test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


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



test_that("modified rolls are within range", {
  TestDice <- c(20, 100)
  Modifiers <- c(2, 10)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test", Modifiers[i])
    
    for (j in 1:100) {
      obs <- roller$Roll()
      expect_gte(obs, 1)
      expect_lte(obs, TestDice[i])
    }
  }
})