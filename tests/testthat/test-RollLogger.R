test_that("string gets printed after logging it", {
  log <- RollLogger$new()

  expect_output(
    log$Log("You're pretty when I'm drunk")$print(), 
    "You're pretty when I'm drunk")
})



test_that("empty log has a length = 0", {
  expect_identical(RollLogger$new()$Length, 0L)
})

test_that("adding a string increases log length", {
  log <- RollLogger$new()
  
  for (i in 1L:10L)
    expect_identical(
      log$Log("You're pretty when I'm drunk")$Length, 
      i)
})
test_that("exceeding max log length does not increases length, anymore", {
  log <- RollLogger$new(10L)
  
  for (i in 1L:10L)
    expect_identical(
      log$Log("You're pretty when I'm drunk")$Length, 
      i)
  for (i in 1L:10L)
    expect_identical(
      log$Log("You're pretty when I'm drunk")$Length, 
      10L)
})



test_that("AsHtml() works forward", {
  log <- RollLogger$new()
  for (i in 1L:3L)
    log$Log(paste0(i))
  
  expect_match(log$AsHtml(FALSE), "1<br/>2<br/>3")
})
test_that("AsHtml() works reverse", {
  log <- RollLogger$new()
  for (i in 1L:3L)
    log$Log(paste0(i))
  
  expect_match(log$AsHtml(TRUE), "3<br/>2<br/>1")
})