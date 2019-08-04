library(lipidomicsUtils)
context("Generation of Shorthand")

## test for correct shorthand notation of GP
test_that("correct shorthand notation for GPs", {
  
  # glycerolipids (PCs)
  expect_equal(get_gp_shorthand("PC(16:0/16:0(15Me))", level = "structural"), "PC(16:0/17:0)")
  expect_equal(get_gp_shorthand("PC(16:0/16:0(15Me))", level = "molecular"), "PC(16:0_17:0)")
  expect_equal(get_gp_shorthand("PC(16:0/16:0(15Me))", level = "species"), "PC(33:0)")
  
  # glycerolipids (PEs)
  expect_equal(get_gp_shorthand("PE(16:0/16:0(15Me))", level = "structural"), "PE(16:0/17:0)")
  expect_equal(get_gp_shorthand("PE(16:0/16:0(15Me))", level = "molecular"), "PE(16:0_17:0)")
  expect_equal(get_gp_shorthand("PE(16:0/16:0(15Me))", level = "species"), "PE(33:0)")
  
  # glycerolipids (PC-Os)
  expect_equal(get_gp_shorthand("PC(O-16:0/16:0(15Me))", level = "structural"), "PC(O-16:0/17:0)")
  expect_equal(get_gp_shorthand("PC(O-16:0/16:0(15Me))", level = "molecular"), "PC(O-16:0_17:0)")
  expect_equal(get_gp_shorthand("PC(O-16:0/16:0(15Me))", level = "species"), "PC(O-33:0)")
  
  # glycerolipids (PC-Ps)
  expect_equal(get_gp_shorthand("PC(P-16:0/16:0(15Me))", level = "structural"), "PC(P-16:0/17:0)")
  expect_equal(get_gp_shorthand("PC(P-16:0/16:0(15Me))", level = "molecular"), "PC(P-16:0_17:0)")
  expect_equal(get_gp_shorthand("PC(P-16:0/16:0(15Me))", level = "species"), "PC(O-33:1)")
  
  # cardiolipins
  expect_equal(get_gp_shorthand("CL(1'-[20:4(5Z,8Z,11Z,14Z)/20:4(5Z,8Z,11Z,14Z)],3'-[18:1(9Z)/18:1(9Z)])", level = "structural"), "CL(20:4/20:4/18:1/18:1)")
  expect_equal(get_gp_shorthand("CL(1'-[20:4(5Z,8Z,11Z,14Z)/20:4(5Z,8Z,11Z,14Z)],3'-[18:1(9Z)/18:1(9Z)])", level = "molecular"), "CL(18:1_18:1_20:4_20:4)")
  expect_equal(get_gp_shorthand("CL(1'-[20:4(5Z,8Z,11Z,14Z)/20:4(5Z,8Z,11Z,14Z)],3'-[18:1(9Z)/18:1(9Z)])", level = "species"), "CL(76:10)")
  
  # NAPEs
  expect_equal(get_gp_shorthand("NAPE(16:0/18:1(9Z)/18:1(9Z))", level = "structural"), "NAPE(16:0/18:1/18:1)")
  expect_equal(get_gp_shorthand("NAPE(16:0/18:1(9Z)/18:1(9Z))", level = "molecular"), "NAPE(16:0_18:1_18:1)")
  expect_equal(get_gp_shorthand("NAPE(16:0/18:1(9Z)/18:1(9Z))", level = "species"), "NAPE(52:2)")
  
  #CDP-DGs
  
})