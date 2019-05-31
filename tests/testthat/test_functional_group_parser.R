library(lipidomicsUtils)
context("Test functional group parsing")

## tests for correct fatty acyls -----------------------------------------------
test_that("correct isolation of functional groups", {
  
  # carbon number
  expect_equal(get_carbon_number("d16:1(4E)(1OH,3OH)(15Me)"), 17)

  # functionalized fatty acid
  expect_equal(get_hydroxy_number("16:0(2OH)"), 1)
  expect_equal(get_peroxy_number("16:0(2OOH)"), 1)
  
  # sphingoid bases
  expect_equal(get_hydroxy_number("d18:1(1OH,3OH)"), 2)
  
})