library(lipidomicsUtils)
context("ion formula creation")

## tests for exact mass to ion mass --------------------------------------------
test_that("correct ion formula are generated", {
  
  # positive ion mode
  expect_equal(create_ion_formula("C6H12O6", "[M+H]+"), "[C6H13O6]1+")
  expect_equal(create_ion_formula("C6H12O6", "[M+Na]+"), "[C6H12O6Na]1+")
  
  # negative ion mode
  expect_equal(create_ion_formula("C6H12O6", "[M-H]-"), "[C6H11O6]1-")
  
})