library(lipidomicsUtils)
context("Calculation of lipid masses")

## tests correct calculation of acyl masses (intact acid or alcohol) -----------
test_that("correct calculation of lipid masses, PCs", {

  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equal(round(calc_lipid_mass("PC(18:0/18:1(9Z))"), 4), 787.6091)
  
  # mass of PC(O-18:0/18:1(9Z)), LMGP01020202
  expect_equal(round(calc_lipid_mass("PC(O-18:0/18:1(9Z))"), 4), 773.6298)
  
  # mass of PC(P-18:0/18:1(9Z)), LMGP01030013
  expect_equal(round(calc_lipid_mass("PC(P-18:0/18:1(9Z))"), 4), 771.6142)
  
})

## tests correct calculation of acyl masses (intact acid or alcohol) -----------
test_that("correct calculation of lipid masses, PEs", {
  
  # mass of PE(18:0/18:1(9Z)), LMGP02010036
  expect_equal(round(calc_lipid_mass("PE(18:0/18:1(9Z))"), 4), 745.5622)
  
  # mass of PE(O-18:0/18:1(9Z)), LMGP02020047
  expect_equal(round(calc_lipid_mass("PE(O-18:0/18:1(9Z))"), 4), 731.5829)
  
  # mass of PE(P-18:0/18:1(9Z)), LMGP02030004
  expect_equal(round(calc_lipid_mass("PE(P-18:0/18:1(9Z))"), 4), 729.5672)
  
})

## tests correct calculation of acyl masses (intact acid or alcohol) -----------
test_that("correct calculation of lipid masses, PSs", {
  
  # mass of PS(18:0/18:1(9Z)), LMGP03010025
  expect_equal(round(calc_lipid_mass("PS(18:0/18:1(9Z))"), 4), 789.5520)
  
  # mass of PS(O-18:0/18:1(9Z)), LMGP03020027	
  expect_equal(round(calc_lipid_mass("PS(O-18:0/18:1(9Z))"), 4), 775.5727)
  
  # mass of PS(P-18:0/18:1(9Z)), LMGP03030091
  expect_equal(round(calc_lipid_mass("PS(P-18:0/18:1(9Z))"), 4), 773.5571)
  
})

## tests correct calculation of acyl masses (intact acid or alcohol) -----------
test_that("correct calculation of lipid masses, PGs", {
  
  # mass of PG(18:0/18:1(9Z)), LMGP04010037
  expect_equal(round(calc_lipid_mass("PG(18:0/18:1(9Z))"), 4), 776.5567)
  
  # mass of PG(O-18:0/18:1(9Z)), LMGP04020027	
  expect_equal(round(calc_lipid_mass("PG(O-18:0/18:1(9Z))"), 4), 762.5775)
  
  # mass of PG(P-18:0/18:1(9Z)), LMGP04030091
  expect_equal(round(calc_lipid_mass("PG(P-18:0/18:1(9Z))"), 4), 760.5618)
  
})