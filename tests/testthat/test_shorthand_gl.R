library(lipidomicsUtils)
context("Generation of Shorthand")

## test for correct shorthand notation of GL
test_that("correct shorthand notation for GLs", {
  
  # glycerolipids (MGs)
  expect_equal(get_gl_shorthand("MG(16:0(15Me)/0:0/0:0)", level = "structural"), "MG(17:0/0:0/0:0)")
  expect_equal(get_gl_shorthand("MG(16:0(15Me)/0:0/0:0)", level = "molecular"), "MG(0:0_0:0_17:0)")
  expect_equal(get_gl_shorthand("MG(16:0(15Me)/0:0/0:0)", level = "species"), "MG(17:0)")
  
  # glycerolipids (MGs)
  expect_equal(get_gl_shorthand("MG(0:0/16:0(15Me)/0:0)", level = "structural"), "MG(0:0/17:0/0:0)")
  expect_equal(get_gl_shorthand("MG(0:0/16:0(15Me)/0:0)", level = "molecular"), "MG(0:0_0:0_17:0)")
  expect_equal(get_gl_shorthand("MG(0:0/16:0(15Me)/0:0)", level = "species"), "MG(17:0)")
  
  # glycerolipids (DGs)
  expect_equal(get_gl_shorthand("DG(16:0(15Me)/18:1(9Z)/0:0)", level = "structural"), "DG(17:0/18:1/0:0)")
  expect_equal(get_gl_shorthand("DG(16:0(15Me)/18:1(9Z)/0:0)", level = "molecular"), "DG(0:0_17:0_18:1)")
  expect_equal(get_gl_shorthand("DG(16:0(15Me)/18:1(9Z)/0:0)", level = "species"), "DG(35:1)")
  
  # glycerolipids (TGs)
  expect_equal(get_gl_shorthand("TG(16:0(15Me)/18:1(9Z)/20:4(5Z,8Z,11Z,14Z))", level = "structural"), "TG(17:0/18:1/20:4)")
  expect_equal(get_gl_shorthand("TG(16:0(15Me)/18:1(9Z)/20:4(5Z,8Z,11Z,14Z))", level = "molecular"), "TG(17:0_18:1_20:4)")
  expect_equal(get_gl_shorthand("TG(16:0(15Me)/18:1(9Z)/20:4(5Z,8Z,11Z,14Z))", level = "species"), "TG(55:5)")
})