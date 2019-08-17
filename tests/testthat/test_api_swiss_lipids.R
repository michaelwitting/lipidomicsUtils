library(lipidomicsUtils)
context("SwissLipids API")

## tests for exact mass to ion mass --------------------------------------------
test_that("Correct IDs returned", {
  
  # exact inchikey search
  expect_equal(inchikey_to_swissid_exact("PORPENFLTBBHSG-MGBGTMOVSA-L"), "SLM:000000808")
  expect_equal(inchikey_to_swissid_exact("WTJKGGKOPKCXLL-VYOBOKEXSA-N"), "SLM:000000651")

  # inchikey without charge
  expect_equal(inchikey_to_swissid_nocharge("PORPENFLTBBHSG-MGBGTMOVSA-L"), "SLM:000000808")
  expect_equal(inchikey_to_swissid_nocharge("PORPENFLTBBHSG-MGBGTMOVSA-N"), "SLM:000000808")
  
  # inchikey only atom connectivity
  expect_equal(inchikey_to_swissid_structure("LYJWNHGFGVVCAO-IEHWZJNJSA-N"), c("SLM:000035635", "SLM:000035642"))
  
})