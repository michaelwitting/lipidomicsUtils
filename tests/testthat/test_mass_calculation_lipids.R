library(lipidomicsUtils)
context("Calculation of lipid masses")

## tests correct calculation of FA masses  -------------------------------------
test_that("correct calculation of lipid masses, FAs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equal(round(calc_lipid_mass("FA(18:1(9Z))"), 4), 282.2559)
  expect_equal(round(calc_lipid_mass("FA(16:0)"), 4), 256.2402)
  expect_equal(round(calc_lipid_mass("FA(20:4)"), 4), 304.2402)
  
})

## tests correct calculation of CoA masses  ------------------------------------
test_that("correct calculation of lipid masses, CoAs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equal(round(calc_lipid_mass("CoA(18:1(9Z))"), 4), 1031.3605)
  expect_equal(round(calc_lipid_mass("CoA(16:0)"), 4), 1005.3449)
  expect_equal(round(calc_lipid_mass("CoA(20:4)"), 4), 1053.3449)
  
})

## tests correct calculation of NAE masses  ------------------------------------
test_that("correct calculation of lipid masses, NAEs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equal(round(calc_lipid_mass("NAE(18:1(9Z))"), 4), 325.2981)
  expect_equal(round(calc_lipid_mass("NAE(16:0)"), 4), 299.2824)
  expect_equal(round(calc_lipid_mass("NAE(20:4)"), 4), 347.2824)
  
})

## tests correct calculation of PNAE masses  -----------------------------------
test_that("correct calculation of lipid masses, PNAEs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equal(round(calc_lipid_mass("PNAE(18:1(9Z))"), 4), 405.2644)
  expect_equal(round(calc_lipid_mass("PNAE(16:0)"), 4), 379.2488)
  expect_equal(round(calc_lipid_mass("PNAE(20:4)"), 4), 427.2488)
  
})

## tests correct calculation of GPNAE masses  ----------------------------------
test_that("correct calculation of lipid masses, PNAEs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equal(round(calc_lipid_mass("GPNAE(18:1(9Z))"), 4), 479.3012)
  expect_equal(round(calc_lipid_mass("GPNAE(16:0)"), 4), 453.2855)
  expect_equal(round(calc_lipid_mass("GPNAE(20:4)"), 4), 501.2855)
  
})

## tests correct calculation of PC masses  -------------------------------------
test_that("correct calculation of lipid masses, PCs", {

  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equal(round(calc_lipid_mass("PC(18:0/18:1(9Z))"), 4), 787.6091)
  
  # mass of PC(O-18:0/18:1(9Z)), LMGP01020202
  expect_equal(round(calc_lipid_mass("PC(O-18:0/18:1(9Z))"), 4), 773.6298)
  
  # mass of PC(P-18:0/18:1(9Z)), LMGP01030013
  expect_equal(round(calc_lipid_mass("PC(P-18:0/18:1(9Z))"), 4), 771.6142)
  
})

## tests correct calculation of PE masses --------------------------------------
test_that("correct calculation of lipid masses, PEs", {
  
  # mass of PE(18:0/18:1(9Z)), LMGP02010036
  expect_equal(round(calc_lipid_mass("PE(18:0/18:1(9Z))"), 4), 745.5622)
  
  # mass of PE(O-18:0/18:1(9Z)), LMGP02020047
  expect_equal(round(calc_lipid_mass("PE(O-18:0/18:1(9Z))"), 4), 731.5829)
  
  # mass of PE(P-18:0/18:1(9Z)), LMGP02030004
  expect_equal(round(calc_lipid_mass("PE(P-18:0/18:1(9Z))"), 4), 729.5672)
  
})

## tests correct calculation of PS masses --------------------------------------
test_that("correct calculation of lipid masses, PSs", {
  
  # mass of PS(18:0/18:1(9Z)), LMGP03010025
  expect_equal(round(calc_lipid_mass("PS(18:0/18:1(9Z))"), 4), 789.5520)
  
  # mass of PS(O-18:0/18:1(9Z)), LMGP03020027	
  expect_equal(round(calc_lipid_mass("PS(O-18:0/18:1(9Z))"), 4), 775.5727)
  
  # mass of PS(P-18:0/18:1(9Z)), LMGP03030091
  expect_equal(round(calc_lipid_mass("PS(P-18:0/18:1(9Z))"), 4), 773.5571)
  
})

## tests correct calculation of PGS masses -------------------------------------
test_that("correct calculation of lipid masses, PGs", {
  
  # mass of PG(18:0/18:1(9Z)), LMGP04010037
  expect_equal(round(calc_lipid_mass("PG(18:0/18:1(9Z))"), 4), 776.5567)
  
  # mass of PG(O-18:0/18:1(9Z)), LMGP04020027	
  expect_equal(round(calc_lipid_mass("PG(O-18:0/18:1(9Z))"), 4), 762.5775)
  
  # mass of PG(P-18:0/18:1(9Z)), LMGP04030091
  expect_equal(round(calc_lipid_mass("PG(P-18:0/18:1(9Z))"), 4), 760.5618)
  
})