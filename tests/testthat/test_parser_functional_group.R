library(lipidomicsUtils)
context("Test functional group parsing")

## tests for correct fatty acyls -----------------------------------------------
test_that("correct isolation of functional groups", {
  
  # carbon number
  expect_equal(get_carbon_number("d16:1(4E)(1OH,3OH)(15Me)"), 17)
  
  # get double bond number
  expect_equal(get_bond_number("18:2(9Z,12Z)"), 2)

  # functionalized fatty acid
  expect_equal(get_hydroxy_number("16:0(2OH)"), 1)
  expect_equal(get_peroxy_number("16:0(2OOH)"), 1)
  expect_equal(get_peroxy_number("18:0(2OOH,5OH"), 1)
  expect_equal(get_keto_number("16:0(3OH,5O"), 1)
  
  # sphingoid bases
  expect_equal(get_hydroxy_number("d18:1(1OH,3OH)"), 2)
  
  # isolation of functional groups
  expect_equal(get_methyl_branches("d16:1(4E,1OH,3OH,15Me)"), c("15Me"))
  expect_equal(get_double_bonds("d16:1(4E,1OH,3OH,15Me"), c("4E"))
  expect_equal(get_hydroxy_groups("18:1(9Z,3OH[S]"), c("3OH[S]"))
  expect_equal(get_keto_groups("18:1(9Z,3O"), c("3O"))
  expect_equal(get_peroxy_groups("18:0(5OOH,7OH"), c("5OOH"))
  expect_equal(get_amino_groups("15:0(3NH2)"), c("3NH2"))
  
})