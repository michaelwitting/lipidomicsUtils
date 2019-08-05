library(lipidomicsUtils)
context("Test lipid category parser")

## tests for all head groups covered -------------------------------------------
test_that("Headgroups correct", {
  
  # glycerolipids
  expect_equal(get_lipid_category("MG"), "GL")
  expect_equal(get_lipid_category("DG"), "GL")
  expect_equal(get_lipid_category("TG"), "GL")
  
  # glycerophospholipids
  expect_equal(get_lipid_category("PC"), "GP")
  expect_equal(get_lipid_category("PE"), "GP")
  expect_equal(get_lipid_category("PS"), "GP")
  expect_equal(get_lipid_category("PG"), "GP")
  expect_equal(get_lipid_category("PGP"), "GP")
  expect_equal(get_lipid_category("PI"), "GP")
  expect_equal(get_lipid_category("PIP"), "GP")
  expect_equal(get_lipid_category("PIP2"), "GP")
  expect_equal(get_lipid_category("PIP3"), "GP")
  expect_equal(get_lipid_category("PA"), "GP")
  expect_equal(get_lipid_category("PPA"), "GP")
  expect_equal(get_lipid_category("CL"), "GP")
  expect_equal(get_lipid_category("CDP-DG"), "GP")
  
  # sphingolipids
  expect_equal(get_lipid_category("SPH"), "SP")
  expect_equal(get_lipid_category("Cer"), "SP")
  expect_equal(get_lipid_category("S1P"), "SP")
  expect_equal(get_lipid_category("CerP"), "SP")
  expect_equal(get_lipid_category("SM"), "SP")
  expect_equal(get_lipid_category("GlcCer"), "SP")
  expect_equal(get_lipid_category("GalCer"), "SP")
  expect_equal(get_lipid_category("LacCer"), "SP")
  expect_equal(get_lipid_category("HexCer"), "SP")
  expect_equal(get_lipid_category("Hex2Cer"), "SP")

})

## tests for PC ----------------------------------------------------------------
test_that("Category parser, PCs", {
  expect_equal(get_lipid_category("PC(16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("PC(16:0/16:1(9Z))"), "GP")
  expect_equal(get_lipid_category("PC(P-16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("PC(O-16:0/16:0)"), "GP")
})

test_that("Class parser, PCs", {
  expect_equal(get_lipid_mainclass("PC(16:0/16:0)"), "PC")
  expect_equal(get_lipid_mainclass("PC(16:0/16:1(9Z))"), "PC")
  expect_equal(get_lipid_mainclass("PC(P-16:0/16:0)"), "PC")
  expect_equal(get_lipid_mainclass("PC(O-16:0/16:0)"), "PC")
})

test_that("Subclass parser, PCs", {
  expect_equal(get_lipid_subclass("PC(16:0/16:0)"), "PC")
  expect_equal(get_lipid_subclass("PC(16:0/16:1(9Z))"), "PC")
  expect_equal(get_lipid_subclass("PC(P-16:0/16:0)"), "PC-P")
  expect_equal(get_lipid_subclass("PC(O-16:0/16:0)"), "PC-O")
})

## tests for PE ----------------------------------------------------------------
test_that("Category parser, PEs", {
  expect_equal(get_lipid_category("PE(16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("PE(16:0/16:1(9Z))"), "GP")
  expect_equal(get_lipid_category("PE(P-16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("PE(O-16:0/16:0)"), "GP")
})

test_that("Class parser, PEs", {
  expect_equal(get_lipid_mainclass("PE(16:0/16:0)"), "PE")
  expect_equal(get_lipid_mainclass("PE(16:0/16:1(9Z))"), "PE")
  expect_equal(get_lipid_mainclass("PE(P-16:0/16:0)"), "PE")
  expect_equal(get_lipid_mainclass("PE(O-16:0/16:0)"), "PE")
})

test_that("Subclass parser, PEs", {
  expect_equal(get_lipid_subclass("PE(16:0/16:0)"), "PE")
  expect_equal(get_lipid_subclass("PE(16:0/16:1(9Z))"), "PE")
  expect_equal(get_lipid_subclass("PE(P-16:0/16:0)"), "PE-P")
  expect_equal(get_lipid_subclass("PE(O-16:0/16:0)"), "PE-O")
})

## tests for PS ----------------------------------------------------------------
test_that("Category parser, PSs", {
  expect_equal(get_lipid_category("PS(16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("PS(16:0/16:1(9Z))"), "GP")
  expect_equal(get_lipid_category("PS(P-16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("PS(O-16:0/16:0)"), "GP")
})

test_that("Class parser, PSs", {
  expect_equal(get_lipid_mainclass("PS(16:0/16:0)"), "PS")
  expect_equal(get_lipid_mainclass("PS(16:0/16:1(9Z))"), "PS")
  expect_equal(get_lipid_mainclass("PS(P-16:0/16:0)"), "PS")
  expect_equal(get_lipid_mainclass("PS(O-16:0/16:0)"), "PS")
})

test_that("Subclass parser, PSs", {
  expect_equal(get_lipid_subclass("PS(16:0/16:0)"), "PS")
  expect_equal(get_lipid_subclass("PS(16:0/16:1(9Z))"), "PS")
  expect_equal(get_lipid_subclass("PS(P-16:0/16:0)"), "PS-P")
  expect_equal(get_lipid_subclass("PS(O-16:0/16:0)"), "PS-O")
})

## tests for CDP-DG ------------------------------------------------------------
test_that("Category parser, CDP-DGs", {
  expect_equal(get_lipid_category("CDP-DG(16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("CDP-DG(16:0/16:1(9Z))"), "GP")
  expect_equal(get_lipid_category("CDP-DG(P-16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("CDP-DG(O-16:0/16:0)"), "GP")
})

test_that("Class parser, CDP-DGs", {
  expect_equal(get_lipid_mainclass("CDP-DG(16:0/16:0)"), "CDP-DG")
  expect_equal(get_lipid_mainclass("CDP-DG(16:0/16:1(9Z))"), "CDP-DG")
  expect_equal(get_lipid_mainclass("CDP-DG(P-16:0/16:0)"), "CDP-DG")
  expect_equal(get_lipid_mainclass("CDP-DG(O-16:0/16:0)"), "CDP-DG")
})

test_that("Subclass parser, CDP-DGs", {
  expect_equal(get_lipid_subclass("CDP-DG(16:0/16:0)"), "CDP-DG")
  expect_equal(get_lipid_subclass("CDP-DG(16:0/16:1(9Z))"), "CDP-DG")
  expect_equal(get_lipid_subclass("CDP-DG(P-16:0/16:0)"), "CDP-DG-P")
  expect_equal(get_lipid_subclass("CDP-DG(O-16:0/16:0)"), "CDP-DG-O")
})

## tests for MG ----------------------------------------------------------------
test_that("Category parser, MGs", {
  expect_equal(get_lipid_category("MG(16:0/0:0/0:0)"), "GL")
  expect_equal(get_lipid_category("MG(P-16:0/0:0/0:0)"), "GL")
  expect_equal(get_lipid_category("MG(O-16:0/0:0/0:0)"), "GL")
})

test_that("Class parser, MGs", {
  expect_equal(get_lipid_mainclass("MG(16:0/0:0/0:0)"), "MG")
  expect_equal(get_lipid_mainclass("MG(P-16:0/0:0/0:0)"), "MG")
  expect_equal(get_lipid_mainclass("MG(O-16:0/0:0/0:0)"), "MG")
})

test_that("Subclass parser, MGs", {
  expect_equal(get_lipid_subclass("MG(16:0/0:0/0:0)"), "MG")
  expect_equal(get_lipid_subclass("MG(P-16:0/0:0/0:0)"), "MG-P")
  expect_equal(get_lipid_subclass("MG(O-16:0/0:0/0:0)"), "MG-O")
})

## tests for DG ----------------------------------------------------------------
test_that("Category parser, DGs", {
  expect_equal(get_lipid_category("DG(16:0/16:0/0:0)"), "GL")
  expect_equal(get_lipid_category("DG(P-16:0/16:0/0:0)"), "GL")
  expect_equal(get_lipid_category("DG(O-16:0/16:0/0:0)"), "GL")
})

test_that("Class parser, DGs", {
  expect_equal(get_lipid_mainclass("DG(16:0/16:0/0:0)"), "DG")
  expect_equal(get_lipid_mainclass("DG(P-16:0/16:0/0:0)"), "DG")
  expect_equal(get_lipid_mainclass("DG(O-16:0/16:0/0:0)"), "DG")
})

test_that("Subclass parser, DGs", {
  expect_equal(get_lipid_subclass("DG(16:0/16:0/0:0)"), "DG")
  expect_equal(get_lipid_subclass("DG(P-16:0/16:0/0:0)"), "DG-P")
  expect_equal(get_lipid_subclass("DG(O-16:0/16:0/0:0)"), "DG-O")
})

## tests for TG ----------------------------------------------------------------
test_that("Category parser, TGs", {
  expect_equal(get_lipid_category("TG(16:0/16:0/16:0)"), "GL")
  expect_equal(get_lipid_category("TG(P-16:0/16:0/16:0)"), "GL")
  expect_equal(get_lipid_category("TG(O-16:0/16:0/16:0)"), "GL")
})

test_that("Class parser, TGs", {
  expect_equal(get_lipid_mainclass("TG(16:0/16:0/16:0)"), "TG")
  expect_equal(get_lipid_mainclass("TG(16:0/16:0/16:0)"), "TG")
  expect_equal(get_lipid_mainclass("TG(P-16:0/16:0/16:0)"), "TG")
  expect_equal(get_lipid_mainclass("TG(O-16:0/16:0/16:0)"), "TG")
})

test_that("Subclass parser, TGs", {
  expect_equal(get_lipid_subclass("TG(16:0/16:0/16:0)"), "TG")
  expect_equal(get_lipid_subclass("TG(16:0/16:0/16:0)"), "TG")
  expect_equal(get_lipid_subclass("TG(P-16:0/16:0/16:0)"), "TG-P")
  expect_equal(get_lipid_subclass("TG(O-16:0/16:0/16:0)"), "TG-O")
})

## tests for Cer ---------------------------------------------------------------
test_that("Category parser, Cers", {
  expect_equal(get_lipid_category("Cer(d18:1/20:0"), "SP")
})

test_that("Class parser, Cers", {
  expect_equal(get_lipid_mainclass("Cer(d18:1/20:0"), "Cer")
})

test_that("Subclass parser, Cers", {
  expect_equal(get_lipid_subclass("Cer(d18:1/20:0"), "Cer")
})

## tests for Hex2Cer ---------------------------------------------------------------
test_that("Category parser, Hex2Cers", {
  expect_equal(get_lipid_category("Hex2Cer(d18:1/20:0"), "SP")
})

test_that("Class parser, Hex2Cers", {
  expect_equal(get_lipid_mainclass("Hex2Cer(d18:1/20:0"), "Hex2Cer")
})

test_that("Subclass parser, Hex2Cers", {
  expect_equal(get_lipid_subclass("Hex2Cer(d18:1/20:0"), "Hex2Cer")
})
