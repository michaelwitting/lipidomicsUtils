library(lipidomicsUtils)
context("Adduct calculation")

## tests for exact mass to ion mass --------------------------------------------
test_that("correct adduct masses are calculated", {
  
  # positive mode, single charge
  expect_equal(round(calc_adduct_mass(731.5465, "[M+H]+"), 4), 732.5538)
  expect_equal(round(calc_adduct_mass(731.5465, "[M+Na]+"), 4), 754.5357)

  # negative mode, single charge
  expect_equal(round(calc_adduct_mass(1352.9722, "[M-H]-"), 4), 1351.9650)
  
  # negative mode, multiple charge
  expect_equal(round(calc_adduct_mass(1352.9722, "[M-2H]2-"), 4), 675.4788)

})