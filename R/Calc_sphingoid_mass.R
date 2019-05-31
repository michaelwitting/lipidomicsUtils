#' @title Calculation of sphingoid base mass (intact sphingoid base)
#' 
#' This function calculates the mass of a sphingnoid base given as shorthand notation, e.g. "d16:1(4E)(15Me)" and calculates the mass of the respective sphingoid base. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#' 
#' @param x Shorthand notation of a sphingoid base (as string), e.g. "d16:1(4E)(15Me)"
#' @example 
#' calc_sphingoid_mass("d16:1(4E)(15Me)")
#'
#' @export
calc_sphingoid_mass <- function(x) {
  
  # get number of carbons, etc...
  carbon_number <- get_carbon_number(x)
  bond_number <- get_bond_number(x)
  
  # get modifications
  peroxy_number <- get_peroxy_number(x)
  hydroxy_number <- get_hydroxy_number(x)
  keto_number <- get_keto_number(x)
  amino_number <- get_amino_number(x)

  # calculate number of atoms (other than C)
  c_count <- carbon_number
  h_count <- 2 * carbon_number - 2 * bond_number - 2 * keto_number + amino_number + 2
  o_count <- hydroxy_number + keto_number + 2 * peroxy_number
  n_count <- amino_number

  # calculate sphingoid mass
  sphingoid_mass <- c_count * lipidomicsUtils:::c_mass +
    h_count * lipidomicsUtils:::h_mass +
    o_count * lipidomicsUtils:::o_mass +
    n_count * lipidomicsUtils:::n_mass
  
  # return sphingoid mass
  return(sphingoid_mass)
}