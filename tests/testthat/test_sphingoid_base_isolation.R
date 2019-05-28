library(lipidomicsUtils)
context("Test sphingoid base parser")

## tests for correct fatty acyls -----------------------------------------------
test_that("correct sphingoid base is isolated", {

  # sphingolipids
  expect_equal(isolate_sphingoid_base("Cer(d18:1/20:0)"), c("d18:1"))
  expect_equal(isolate_sphingoid_base("Cer(d18:1/20:0(2OH))"), c("d18:1"))
  expect_equal(isolate_sphingoid_base("Cer(d16:1(15Me)/20:0(2OH))"), c("d16:1(15Me)"))

})