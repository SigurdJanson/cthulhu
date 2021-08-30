

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



test_that("skill roller is correctly identified", {
  roller <- CthulhuRoller$new(100, "Test", 10)
  
  expect_true(roller$IsSkillRoller)
})

test_that("skill roller is denied when die sides != 100", {
  TestDice <- c(2, 50, 99, 101, 1000)
  Modifiers <- c(1, 10, 9,  10, 10)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test", Modifiers[i])
    expect_false(roller$IsSkillRoller)
  }
})




test_that("MaxHardSuccess works", {
  TestDice  <- c(4, 10, 12, 100, 1000)
  Modifiers <- c(1,  2,  2,  10,   10)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test", Modifiers[i])
    
    RollValues <- c(1, (TestDice[i]-2) %/% 2, TestDice[i] %/% 2)
    Successes  <- RollValues * 2
    Successes[Successes > TestDice[i]-1]  <- NA
    
    for (r in 1:length(RollValues))
      if (!is.na(Successes[r]))
        expect_identical(roller$MaxHardSuccess(RollValues[r]), Successes[r])
      else
        expect_equal(roller$MaxHardSuccess(RollValues[r]), NA)
  }
})


test_that("MaxExtremeSuccess works", {
  TestDice  <- c(4, 10, 12, 100, 1000)
  Modifiers <- c(1,  2,  2,  10,   10)
  
  for(i in 1:length(TestDice)) {
    roller <- CthulhuRoller$new(TestDice[i], "Test", Modifiers[i])
    
    RollValues <- c(1, (TestDice[i]-5) %/% 5, TestDice[i] %/% 5)
    Successes  <- RollValues * 5
    Successes[Successes > TestDice[i]-1]  <- NA
    
    for (r in 1:length(RollValues))
      if (!is.na(Successes[r]))
        expect_identical(roller$MaxExtremeSuccess(RollValues[r]), Successes[r])
      else
        expect_equal(roller$MaxExtremeSuccess(RollValues[r]), NA)
  }
})


