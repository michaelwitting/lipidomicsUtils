library(lipidomicsUtils)
context("Acyl and sphingoid mass calculation")

## tests correct calculation of acyl masses (intact acid or alcohol) -----------
test_that("correct calculation of acyl masses", {
  
  # simple acyls
  expect_equal(round(calc_intact_acyl_mass("16:0"), 4), 256.2402)
  expect_equal(round(calc_intact_acyl_mass("16:0(15Me)"), 4), 270.2559)
  expect_equal(round(calc_intact_acyl_mass("18:0"), 4), 284.2715)
  
  # MUFA and PUFA
  expect_equal(round(calc_intact_acyl_mass("18:1(9Z)"), 4), 282.2559)
  expect_equal(round(calc_intact_acyl_mass("20:4(5Z,8Z,11Z,14Z)"), 4), 304.2402)
  
  # oxidized lipids
  expect_equal(round(calc_intact_acyl_mass("20:4(7E,9E,11Z,14Z)(5OH[S],6OH[R])"), 4), 336.2301)
  
  # alkyls and alkenyls
  expect_equal(round(calc_intact_acyl_mass("O-18:0"), 4), 270.2923)
  expect_equal(round(calc_intact_acyl_mass("P-18:0"), 4), 268.2766)
  
})

## tests correct calculation of sphingoid masses
test_that("correct calculation of sphingoid masses", {
  
  # sphingolipids, full structure details
  expect_equal(round(calc_sphingoid_mass("d18:1(4E)(1OH,3OH)"), 4), 299.2824)
  expect_equal(round(calc_sphingoid_mass("d16:1(4E)(1OH,3OH)(15Me)"), 4), 285.2668)
  
  # sphingolipids, hydroxyl group level
  expect_equal(round(calc_sphingoid_mass("d18:1"), 4), 299.2824)
  expect_equal(round(calc_sphingoid_mass("d17:1"), 4), 285.2668)
  
})