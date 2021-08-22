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
    
    expect_equal(roller$die, DieList[i])
    expect_equal(roller$label, LabelList[i])
    expect_equal(roller$modrolls, ModsAllowed[i])
    
    if (ModsAllowed[i])
      expect_equal(roller$moddice, ModDieList[[i]])
    else
      expect_null(roller$moddice)
  }
})


test_that("rolls are within range", {
  TestDice <- c(3, 4, 6, 10, 20, 100)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test")
    
    for (j in 1:100) {
      obs <- roller$roll()
      expect_gte(obs, 1)
      expect_lte(obs, TestDice[i])
    }
  }
})
