library(lipidomicsUtils)
context("Test fatty acyl parser")

## tests for correct fatty acyls -----------------------------------------------
test_that("correct fatty acyls are isolated", {
  
  # glycerolipids
  expect_equal(isolate_fatty_acyls("MG(16:0/0:0/0:0)"), c("16:0", "0:0", "0:0"))
  expect_equal(isolate_fatty_acyls("DG(16:0/18:1(9Z)/0:0)"), c("16:0", "18:1(9Z)", "0:0"))
  expect_equal(isolate_fatty_acyls("TG(16:0/18:1(9Z)/18:0)"), c("16:0", "18:1(9Z)", "18:0"))
  
  # glycerphospholipids
  expect_equal(isolate_fatty_acyls("PC(16:0/18:1(9Z))"), c("16:0", "18:1(9Z)"))
  expect_equal(isolate_fatty_acyls("PC(16:0/18:3(6Z,9Z,12Z))"), c("16:0", "18:3(6Z,9Z,12Z)"))
  expect_equal(isolate_fatty_acyls("PC(O-16:0/18:1(9Z))"), c("O-16:0", "18:1(9Z)"))
  expect_equal(isolate_fatty_acyls("PC(P-16:0/18:1(9Z))"), c("P-16:0", "18:1(9Z)"))
  expect_equal(isolate_fatty_acyls("PC(P-16:0/16:0(15Me))"), c("P-16:0", "16:0(15Me)"))
  
  # oxidized lipid
  expect_equal(isolate_fatty_acyls("PC(18:0/20:4(7E,9E,11Z,14Z)(5OH[S],6OH[R])"), c("18:0", "20:4(7E,9E,11Z,14Z)(5OH[S],6OH[R])"))
  
  # sphingolipids
  expect_equal(isolate_fatty_acyls("Cer(d18:1/20:0)"), c("20:0"))
  expect_equal(isolate_fatty_acyls("Cer(d18:1/20:0(2OH))"), c("20:0(2OH)"))
  
  # coenzyme A
  expect_equal(isolate_fatty_acyls("CoA(16:1(2E))"), "16:1(2E)")
  
})

## tests for carbon and double bond number -------------------------------------
test_that("correct number of carbons and doubles bonds are isolated", {
  
  ## carbon numbers
  # saturated, straight chain
  expect_equal(get_carbon_number("16:0"), 16)
  expect_equal(get_carbon_number("17:0"), 17)
  expect_equal(get_carbon_number("30:0"), 30)
  
  # saturated, iso chain
  expect_equal(get_carbon_number("14:0(13Me)"), 15)
  expect_equal(get_carbon_number("16:0(15Me)"), 17)
  
  # mono unsaturated
  expect_equal(get_carbon_number("16:1(9Z)"), 16)
  expect_equal(get_carbon_number("18:1(9Z)"), 18)
  
  # poly unsaturated
  expect_equal(get_carbon_number("18:3(6Z,9Z,12Z)"), 18)
  
  ## double bonds
  # saturated, straight chain
  expect_equal(get_bond_number("16:0"), 0)
  expect_equal(get_bond_number("17:0"), 0)
  expect_equal(get_bond_number("30:0"), 0)
  
  # saturated, iso chain
  expect_equal(get_bond_number("14:0(13Me)"), 0)
  expect_equal(get_bond_number("16:0(15Me)"), 0)
  
  # mono unsaturated
  expect_equal(get_bond_number("16:1(9Z)"), 1)
  expect_equal(get_bond_number("18:1(9Z)"), 1)
  
  # poly unsaturated
  expect_equal(get_bond_number("18:3(6Z,9Z,12Z)"), 3)
  
})

## tests for correct fatty acyls (new radyl function) --------------------------
test_that("correct fatty acyls are isolated", {
  
  # glycerolipids (single lipid)
  expect_equal(isolate_radyls("MG(16:0/0:0/0:0)"), list(c("16:0", "0:0", "0:0")))
  expect_equal(isolate_radyls("DG(16:0/18:1(9Z)/0:0)"), list(c("16:0", "18:1(9Z)", "0:0")))
  expect_equal(isolate_radyls("TG(16:0/18:1(9Z)/18:0)"), list(c("16:0", "18:1(9Z)", "18:0")))
  
  # glycerolipids (multiple lipids)
  expect_equal(isolate_radyls(c("MG(16:0/0:0/0:0)",
                                "DG(16:0/18:1(9Z)/0:0)",
                                "TG(16:0/18:1(9Z)/18:0)")), list(c("16:0", "0:0", "0:0"),
                                                                 c("16:0", "18:1(9Z)", "0:0"),
                                                                 c("16:0", "18:1(9Z)", "18:0")))
  
  # glycerphospholipids (single lipid)
  expect_equal(isolate_radyls("PC(16:0/18:1(9Z))"), list(c("16:0", "18:1(9Z)")))
  expect_equal(isolate_radyls("PC(16:0/18:3(6Z,9Z,12Z))"), list(c("16:0", "18:3(6Z,9Z,12Z)")))
  expect_equal(isolate_radyls("PC(O-16:0/18:1(9Z))"), list(c("O-16:0", "18:1(9Z)")))
  expect_equal(isolate_radyls("PC(P-16:0/18:1(9Z))"), list(c("P-16:0", "18:1(9Z)")))
  expect_equal(isolate_radyls("PC(P-16:0/16:0(15Me))"), list(c("P-16:0", "16:0(15Me)")))
  
  # glycerphospholipids (multiple lipids)
  expect_equal(isolate_radyls(c("PC(16:0/18:1(9Z))",
                                "PC(16:0/18:3(6Z,9Z,12Z))",
                                "PC(O-16:0/18:1(9Z))",
                                "PC(P-16:0/18:1(9Z))",
                                "PC(P-16:0/16:0(15Me))")), list(c("16:0", "18:1(9Z)"),
                                                                c("16:0", "18:3(6Z,9Z,12Z)"),
                                                                c("O-16:0", "18:1(9Z)"),
                                                                c("P-16:0", "18:1(9Z)"),
                                                                c("P-16:0", "16:0(15Me)")))
  
  # oxidized lipid(single lipid)
  expect_equal(isolate_radyls("PC(18:0/20:4(7E,9E,11Z,14Z)(5OH[S],6OH[R])"), list(c("18:0", "20:4(7E,9E,11Z,14Z)(5OH[S],6OH[R])")))
  
  # sphingolipids (single lipid)
  expect_equal(isolate_radyls("Cer(d18:1/20:0)"), list(c("20:0")))
  expect_equal(isolate_radyls("Cer(d18:1/20:0(2OH))"), list(c("20:0(2OH)")))
  
  # coenzyme A (single lipid)
  expect_equal(isolate_radyls("CoA(16:1(2E))"), list(c("16:1(2E)")))
  
})
