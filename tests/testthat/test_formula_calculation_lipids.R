library(lipidomicsUtils)
context("Calculation of lipid formulae")

## tests correct calculation of FA formula  -------------------------------------
test_that("correct calculation of lipid formulae, FAs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("FA(18:1(9Z))"), "C18H34O2")
  expect_equivalent(calc_lipid_formulae("FA(16:0)"), "C16H32O2")
  expect_equivalent(calc_lipid_formulae("FA(20:4)"), "C20H32O2")
  
})

## tests correct calculation of CoA formula  ------------------------------------
test_that("correct calculation of lipid formula, CoAs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("CoA(18:1(9Z))"), "C39H68N7O17SP3")
  expect_equivalent(calc_lipid_formulae("CoA(16:0)"), "C37H66N7O17SP3")
  expect_equivalent(calc_lipid_formulae("CoA(20:4)"), "C41H66N7O17SP3")
  
})

## tests correct calculation of NAE formula  ------------------------------------
test_that("correct calculation of lipid formula, NAEs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("NAE(18:1(9Z))"), "C20H39NO2")
  expect_equivalent(calc_lipid_formulae("NAE(16:0)"), "C18H37NO2")
  expect_equivalent(calc_lipid_formulae("NAE(20:4)"), "C22H37NO2")
  
})

## tests correct calculation of PNAE formula  -----------------------------------
test_that("correct calculation of lipid formula, PNAEs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("PNAE(18:1(9Z))"), "C20H40NO5P")
  expect_equivalent(calc_lipid_formulae("PNAE(16:0)"), "C18H38NO5P")
  expect_equivalent(calc_lipid_formulae("PNAE(20:4)"), "C22H38NO5P")
  
})

## tests correct calculation of GPNAE formula  ----------------------------------
test_that("correct calculation of lipid formula, GPNAEs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("GPNAE(18:1(9Z))"), "C23H46NO7P")
  expect_equivalent(calc_lipid_formulae("GPNAE(16:0)"), "C21H44NO7P")
  expect_equivalent(calc_lipid_formulae("GPNAE(20:4)"), "C25H44NO7P")
  
})

## tests correct calculation of PC formula  -------------------------------------
test_that("correct calculation of lipid formula, PCs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("PC(18:0/18:1(9Z))"), "C44H86NO8P")
  
  # mass of PC(O-18:0/18:1(9Z)), LMGP01020202
  expect_equivalent(calc_lipid_formulae("PC(O-18:0/18:1(9Z))"), "C44H88NO7P")
  
  # mass of PC(P-18:0/18:1(9Z)), LMGP01030013
  expect_equivalent(calc_lipid_formulae("PC(P-18:0/18:1(9Z))"), "C44H86NO7P")
  
})

## tests correct calculation of PE formula --------------------------------------
test_that("correct calculation of lipid formula, PEs", {
  
  # mass of PE(18:0/18:1(9Z)), LMGP02010036
  expect_equivalent(calc_lipid_formulae("PE(18:0/18:1(9Z))"), "C41H80NO8P" )
  
  # mass of PE(O-18:0/18:1(9Z)), LMGP02020047
  expect_equivalent(calc_lipid_formulae("PE(O-18:0/18:1(9Z))"), "C41H82NO7P")
  
  # mass of PE(P-18:0/18:1(9Z)), LMGP02030004
  expect_equivalent(calc_lipid_formulae("PE(P-18:0/18:1(9Z))"), "C41H80NO7P")
  
})

## tests correct calculation of PS formula --------------------------------------
test_that("correct calculation of lipid formula, PSs", {
  
  # mass of PS(18:0/18:1(9Z)), LMGP03010025
  expect_equivalent(calc_lipid_formulae("PS(18:0/18:1(9Z))"), "C42H80NO10P")
  
  # mass of PS(O-18:0/18:1(9Z)), LMGP03020027	
  expect_equivalent(calc_lipid_formulae("PS(O-18:0/18:1(9Z))"), "C42H82NO9P")
  
  # mass of PS(P-18:0/18:1(9Z)), LMGP03030091
  expect_equivalent(calc_lipid_formulae("PS(P-18:0/18:1(9Z))"), "C42H80NO9P")
  
})

## tests correct calculation of PGS formula -------------------------------------
test_that("correct calculation of lipid formula, PGs", {
  
  # mass of PG(18:0/18:1(9Z)), LMGP04010037
  expect_equivalent(calc_lipid_formulae("PG(18:0/18:1(9Z))"), "C42H81O10P")
  
  # mass of PG(O-18:0/18:1(9Z)), LMGP04020027	
  expect_equivalent(calc_lipid_formulae("PG(O-18:0/18:1(9Z))"), "C42H83O9P")
  
  # mass of PG(P-18:0/18:1(9Z)), LMGP04030091
  expect_equivalent(calc_lipid_formulae("PG(P-18:0/18:1(9Z))"), "C42H81O9P")
  
})

## tests correct calculation of PA formula -------------------------------------
test_that("correct calculation of lipid formula, PAs", {
  
  # mass of PG(18:0/18:1(9Z)), LMGP04010037
  expect_equivalent(calc_lipid_formulae("PA(18:0/18:1(9Z))"), "C39H75O8P")
  
  # mass of PG(O-18:0/18:1(9Z)), LMGP04020027	
  expect_equivalent(calc_lipid_formulae("PA(O-18:0/18:1(9Z))"), "C39H77O7P")
  
  # mass of PG(P-18:0/18:1(9Z)), LMGP04030091
  expect_equivalent(calc_lipid_formulae("PA(P-18:0/18:1(9Z))"), "C39H75O7P")
  
})

## tests correct calculation of PI formula -------------------------------------
test_that("correct calculation of lipid formula, PIs", {
  
  # mass of PG(18:0/18:1(9Z)), LMGP04010037
  expect_equivalent(calc_lipid_formulae("PI(18:0/18:1(9Z))"), "C45H85O13P")
  
  # mass of PG(O-18:0/18:1(9Z)), LMGP04020027	
  expect_equivalent(calc_lipid_formulae("PI(O-18:0/18:1(9Z))"), "C45H87O12P")
  
  # mass of PG(P-18:0/18:1(9Z)), LMGP04030091
  expect_equivalent(calc_lipid_formulae("PI(P-18:0/18:1(9Z))"), "C45H85O12P")
  
})

## tests correct calculation of PI formula -------------------------------------
test_that("correct calculation of lipid formula, PIPs", {
  
  # mass of PG(18:0/18:1(9Z)), LMGP04010037
  expect_equivalent(calc_lipid_formulae("PIP(16:0/18:1(9Z))"), "C43H82O16P2")
  
  # mass of PG(O-18:0/18:1(9Z)), LMGP04020027	
  expect_equivalent(calc_lipid_formulae("PIP(O-16:0/18:1(9Z))"), "C43H84O15P2")
  
  # mass of PG(P-18:0/18:1(9Z)), LMGP04030091
  expect_equivalent(calc_lipid_formulae("PIP(P-16:0/18:1(9Z))"), "C43H82O15P2")
  
})

## tests correct calculation of MG formula  -------------------------------------
test_that("correct calculation of lipid formula, MGs, DGs, TGs", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("MG(18:0/0:0/0:0)"), "C21H42O4")
  expect_equivalent(calc_lipid_formulae("DG(18:0/18:0/0:0)"), "C39H76O5")
  expect_equivalent(calc_lipid_formulae("TG(18:0/18:0/18:1(9Z))"), "C57H108O6")
  
  # mass of PC(O-18:0/18:1(9Z)), LMGP01020202
  expect_equivalent(calc_lipid_formulae("MG(O-18:0/0:0/0:0)"), "C21H44O3")
  expect_equivalent(calc_lipid_formulae("DG(O-18:0/18:0/0:0)"), "C39H78O4")
  expect_equivalent(calc_lipid_formulae("TG(O-18:0/18:0/18:1(9Z))"), "C57H110O5")
  
  # mass of PC(P-18:0/18:1(9Z)), LMGP01030013
  expect_equivalent(calc_lipid_formulae("MG(P-18:0/0:0/0:0)"), "C21H42O3")
  expect_equivalent(calc_lipid_formulae("DG(P-18:0/18:0/0:0)"), "C39H76O4")
  expect_equivalent(calc_lipid_formulae("TG(P-18:0/18:0/18:1(9Z))"), "C57H108O5")
  
})

## tests correct calculation of Cer formula  -----------------------------------
test_that("correct calculation of lipid formula, Cer", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("Cer(d18:1/18:0)"), "C36H71NO3")
  expect_equivalent(calc_lipid_formulae("Cer(d18:1/18:0(2OH))"), "C36H71NO4")
  
})

## tests correct calculation of SM formula  ------------------------------------
test_that("correct calculation of lipid formula, SM", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("SM(d18:1/18:0)"), "C41H83N2O6P")
  expect_equivalent(calc_lipid_formulae("SM(d18:1/18:0(2OH))"), "C41H83N2O7P")
  
})

## tests correct calculation of SM formula  ------------------------------------
test_that("correct calculation of lipid formula, HexCer", {
  
  # mass of PC(18:0/18:1(9Z)), LMGP01010761
  expect_equivalent(calc_lipid_formulae("HexCer(d18:1/18:0)"), "C42H81NO8")
  expect_equivalent(calc_lipid_formulae("HexCer(d18:1/18:0(2OH))"), "C42H81NO9")
  
})