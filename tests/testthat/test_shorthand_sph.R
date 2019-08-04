library(lipidomicsUtils)
context("Generation of Shorthand")

## test for correct shorthand notation of GP
test_that("correct shorthand notation for SPHs", {
  
  # glycerolipids (PCs)
  expect_equal(get_sph_shorthand("GlcCer(d16:0(15Me)(1OH,4OH)/22:0(2OH[R])", level = "position"), "HexCer(d17:0/22:0(OH))")

  
})
