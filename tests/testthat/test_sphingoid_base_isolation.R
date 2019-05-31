library(lipidomicsUtils)
context("Test sphingoid base parser")

## tests for correct sphingoid bases -------------------------------------------
test_that("correct sphingoid base is isolated", {

  # sphingolipids, full structure details
  expect_equal(isolate_sphingoid_base("Cer(d18:1(4E)(1OH,3OH)/20:0)"), c("d18:1(4E)(1OH,3OH)"))
  expect_equal(isolate_sphingoid_base("Cer(d18:1(4E)(1OH,3OH)/20:0(2OH))"), c("d18:1(4E)(1OH,3OH)"))
  expect_equal(isolate_sphingoid_base("Cer(d16:1(4E)(1OH,3OH)(15Me)/20:0(2OH))"), c("d16:1(4E)(1OH,3OH)(15Me)"))
  
  # sphingolipids, hydroxyl group level
  expect_equal(isolate_sphingoid_base("Cer(d18:1/20:0)"), c("d18:1"))
  expect_equal(isolate_sphingoid_base("Cer(d17:1/22:0)"), c("d17:1"))

})