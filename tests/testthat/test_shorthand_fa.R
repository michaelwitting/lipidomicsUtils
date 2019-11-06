library(lipidomicsUtils)
context("Generation of Shorthand")

## test for correct shorthand notation of FA
test_that("correct shorthand notation for FAs", {
  
  # saturated fatty acids
  expect_equal(get_fa_shorthand("FA(16:0)", level = "structural"), "FA(16:0)")
  expect_equal(get_fa_shorthand("FA(16:0)", level = "molecular"), "FA(16:0)")
  expect_equal(get_fa_shorthand("FA(16:0)", level = "species"), "FA(16:0)")
  
  # saturated, iso branched fatty acids
  expect_equal(get_fa_shorthand("FA(16:0(15Me))", level = "structural"), "FA(17:0)")
  expect_equal(get_fa_shorthand("FA(16:0(15Me))", level = "molecular"), "FA(17:0)")
  expect_equal(get_fa_shorthand("FA(16:0(15Me))", level = "species"), "FA(17:0)")
  
  # mono-unsaturated fatty acids
  expect_equal(get_fa_shorthand("FA(18:1(9Z))", level = "structural"), "FA(18:1)")
  expect_equal(get_fa_shorthand("FA(18:1(9Z))", level = "molecular"), "FA(18:1)")
  expect_equal(get_fa_shorthand("FA(18:1(9Z))", level = "species"), "FA(18:1)")
  
  # poly-unsaturated fatty acids
  expect_equal(get_fa_shorthand("FA(20:4(5Z,8Z,11Z,14Z))", level = "structural"), "FA(20:4)")
  expect_equal(get_fa_shorthand("FA(20:4(5Z,8Z,11Z,14Z))", level = "molecular"), "FA(20:4)")
  expect_equal(get_fa_shorthand("FA(20:4(5Z,8Z,11Z,14Z))", level = "species"), "FA(20:4)")
  
  # n-acyl ethanolamines
  expect_equal(get_fa_shorthand("NAE(20:4(5Z,8Z,11Z,14Z))", level = "structural"), "NAE(20:4)")
  expect_equal(get_fa_shorthand("NAE(20:4(5Z,8Z,11Z,14Z))", level = "molecular"), "NAE(20:4)")
  expect_equal(get_fa_shorthand("NAE(20:4(5Z,8Z,11Z,14Z))", level = "species"), "NAE(20:4)")
})


# ## test for correct generation of BiGG IDs
# test_that("correct generation of BiGG IDs", {
#   
#   # saturated acyls
#   expect_equal(get_fa_biggid("FA(16:0)"), "fa16_0")
#   expect_equal(get_fa_biggid("CoA(16:0)"), "coa16_0")
#   
#   # monounsaturated acyls
#   expect_equal(get_fa_biggid("FA(18:1(9Z))"), "fa18_1_9z")
#   
#   # funtionalized
#   expect_equal(get_fa_biggid("CoA(18:1(9Z,3O))"), "coa18_1_9z3o")
#   expect_equal(get_fa_biggid("CoA(18:1(9Z,3OH[S]))"), "coa18_1_9z3oh__S")
#   
# })
