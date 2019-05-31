library(lipidomicsUtils)
context("Calculation of lipid masses")

## tests correct calculation of acyl masses (intact acid or alcohol) -----------
test_that("correct calculation of lipid masses, PCs", {
  

  # mass of PC(18:0/18:1(9Z))
  expect_equal(round(lipidomicsUtils:::gpc_mass +
                       calc_residue_acyl_mass("18:0") +
                       calc_residue_acyl_mass("18:1(9Z)"), 4),
               787.6091)
  
  # mass of PC(O-18:0/18:1(9Z))
  expect_equal(round(lipidomicsUtils:::gpc_mass +
                       calc_residue_acyl_mass("O-18:0") +
                       calc_residue_acyl_mass("18:1(9Z)"), 4),
               773.6298)
  
  # mass of PC(O-18:0/18:1(9Z))
  expect_equal(round(lipidomicsUtils:::gpc_mass +
                       calc_residue_acyl_mass("P-18:0") +
                       calc_residue_acyl_mass("18:1(9Z)"), 4),
               771.6142)
  
})