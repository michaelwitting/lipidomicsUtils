library(lipidomicsUtils)
context("Generation of Shorthand")

## test for correct shorthand notation of GP
test_that("correct shorthand notation for GPs", {
  
  # glycerolipids (PCs)
  expect_equal(get_gp_shorthand("PC(16:0/16:0(15Me))", level = "position"), "PC(16:0/17:0)")
  expect_equal(get_gp_shorthand("PC(16:0/16:0(15Me))", level = "acyl"), "PC(16:0_17:0)")
  expect_equal(get_gp_shorthand("PC(16:0/16:0(15Me))", level = "species"), "PC(33:0)")
  
  # glycerolipids (PEs)
  expect_equal(get_gp_shorthand("PE(16:0/16:0(15Me))", level = "position"), "PE(16:0/17:0)")
  expect_equal(get_gp_shorthand("PE(16:0/16:0(15Me))", level = "acyl"), "PE(16:0_17:0)")
  expect_equal(get_gp_shorthand("PE(16:0/16:0(15Me))", level = "species"), "PE(33:0)")
  
  # glycerolipids (PC-Os)
  expect_equal(get_gp_shorthand("PC(O-16:0/16:0(15Me))", level = "position"), "PC(O-16:0/17:0)")
  expect_equal(get_gp_shorthand("PC(O-16:0/16:0(15Me))", level = "acyl"), "PC(O-16:0_17:0)")
  expect_equal(get_gp_shorthand("PC(O-16:0/16:0(15Me))", level = "species"), "PC(O-33:0)")
  
  # glycerolipids (PC-Ps)
  expect_equal(get_gp_shorthand("PC(P-16:0/16:0(15Me))", level = "position"), "PC(P-16:0/17:0)")
  expect_equal(get_gp_shorthand("PC(P-16:0/16:0(15Me))", level = "acyl"), "PC(P-16:0_17:0)")
  expect_equal(get_gp_shorthand("PC(P-16:0/16:0(15Me))", level = "species"), "PC(O-33:1)")
  
})