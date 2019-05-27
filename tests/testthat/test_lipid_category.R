library(lipidomicsUtils)
context("Test lipid category parser")

## tests for PC ----------------------------------------------------------------
test_that("Category parser, PCs", {
  expect_equal(get_lipid_category("PC(16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("PC(16:0/16:1(9Z))"), "GP")
  expect_equal(get_lipid_category("PC(P-16:0/16:0)"), "GP")
  expect_equal(get_lipid_category("PC(O-16:0/16:0)"), "GP")

})

test_that("Class parser, PCs", {
  expect_equal(get_lipid_class("PC(16:0/16:0)"), "PC")
  expect_equal(get_lipid_class("PC(16:0/16:1(9Z))"), "PC")
  expect_equal(get_lipid_class("PC(P-16:0/16:0)"), "PC")
  expect_equal(get_lipid_class("PC(O-16:0/16:0)"), "PC")
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
  expect_equal(get_lipid_class("PE(16:0/16:0)"), "PE")
  expect_equal(get_lipid_class("PE(16:0/16:1(9Z))"), "PE")
  expect_equal(get_lipid_class("PE(P-16:0/16:0)"), "PE")
  expect_equal(get_lipid_class("PE(O-16:0/16:0)"), "PE")
})

test_that("Subclass parser, PEs", {
  expect_equal(get_lipid_subclass("PE(16:0/16:0)"), "PE")
  expect_equal(get_lipid_subclass("PE(16:0/16:1(9Z))"), "PE")
  expect_equal(get_lipid_subclass("PE(P-16:0/16:0)"), "PE-P")
  expect_equal(get_lipid_subclass("PE(O-16:0/16:0)"), "PE-O")
})