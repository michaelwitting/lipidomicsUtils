library(lipidomicsUtils)
context("Generation of Shorthand")

## test for correct shorthand notation of GP
test_that("correct shorthand notation for SPHs", {
  
  # ceramides
  expect_equal(get_sph_shorthand("Cer(d16:1(4E,1OH,3OH,15Me)/22:1(13Z,2OH[R]))", level = "structural"), "Cer(d17:1/22:1(OH))")
  expect_equal(get_sph_shorthand("Cer(d16:1(4E,1OH,3OH,15Me)/22:1(13Z,2OH[R]))", level = "molecular"), "Cer(d17:1/22:1(OH))")
  expect_equal(get_sph_shorthand("Cer(d16:1(4E,1OH,3OH,15Me)/22:1(13Z,2OH[R]))", level = "species"), "Cer(t39:2)")
  
  # glyco sphingolipids
  expect_equal(get_sph_shorthand("GlcCer(d16:0(15Me)(1OH,4OH)/22:0(2OH[R])", level = "structural"), "HexCer(d17:0/22:0(OH))")
  expect_equal(get_sph_shorthand("GlcCer(d16:0(15Me)(1OH,4OH)/22:0(2OH[R])", level = "molecular"), "HexCer(d17:0/22:0(OH))")
  expect_equal(get_sph_shorthand("GlcCer(d16:0(15Me)(1OH,4OH)/22:0(2OH[R])", level = "species"), "HexCer(t39:0)")
  
})
