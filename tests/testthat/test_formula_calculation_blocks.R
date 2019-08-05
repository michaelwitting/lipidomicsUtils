library(lipidomicsUtils)
context("Acyl and sphingoid formula calculation")

## tests correct calculation of acyl formulaes (intact acid or alcohol) -----------
test_that("correct calculation of acyl formulaes", {
  
  # simple acyls
  expect_equal(calc_intact_acyl_formula("16:0"), "C16H32O2")
  expect_equal(calc_intact_acyl_formula("16:0(15Me)"), "C17H34O2")
  expect_equal(calc_intact_acyl_formula("18:0"), "C18H36O2")
  
  # MUFA and PUFA
  expect_equal(calc_intact_acyl_formula("18:1(9Z)"), "C18H34O2")
  expect_equal(calc_intact_acyl_formula("20:4(5Z,8Z,11Z,14Z)"), "C20H32O2")
  
  # oxidized lipids
  expect_equal(calc_intact_acyl_formula("20:4(7E,9E,11Z,14Z)(5OH[S],6OH[R])"), "C20H32O4")
  
  # alkyls and alkenyls
  expect_equal(calc_intact_acyl_formula("O-18:0"), "C18H38O")
  expect_equal(calc_intact_acyl_formula("P-18:0"), "C18H36O")
  
})

## tests correct calculation of sphingoid formulaes
test_that("correct calculation of sphingoid formulaes", {
  
  # sphingolipids, full structure details
  expect_equal(calc_sphingoid_formula("d18:1(4E)(1OH,3OH)"), "C18H37NO2")
  expect_equal(calc_sphingoid_formula("d16:1(4E)(1OH,3OH)(15Me)"), "C17H35NO2")
  
  # sphingolipids, hydroxyl group level
  expect_equal(calc_sphingoid_formula("d18:1"), "C18H37NO2")
  expect_equal(calc_sphingoid_formula("d17:1"), "C17H35NO2")
  
})