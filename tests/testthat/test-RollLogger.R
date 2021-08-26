test_that("string gets printed after logging it", {
  log <- RollLogger$new()

  expect_output(
    log$Log("You're pretty when I'm drunk")$print(), 
    "You're pretty when I'm drunk")
})


