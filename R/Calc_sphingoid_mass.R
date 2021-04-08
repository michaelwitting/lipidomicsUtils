#' @title Calculation of sphingoid mass
#' 
#' This function calculates the mass of a sphingoid base.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' calc_sphingoid_mass("d16:1(1OH,3OH,15Me)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{calc_sphingoid_formula}}
#'
#' @export
calc_sphingoid_mass <- function(sphingoid_base) {
  
  # calculate formula
  sphingoid_formula <- lipidomicsUtils::calc_sphingoid_formula(sphingoid_base)
  
  # calculate sphingoid mass
  sphingoid_mass <- rcdk::get.formula(sphingoid_formula)@mass
  
  # return sphingoid mass
  return(sphingoid_mass)
}

#' @title Calculation of sphingoid formula
#' 
#' This function calculates the formula of a sphingoid base.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' calc_sphingoid_formula("d16:1(1OH,3OH,15Me)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{calc_sphingoid_mass}}
#'
#' @export
calc_sphingoid_formula <- function(sphingoid_base) {
  
  # get number of carbons, etc...
  carbon_number <- get_carbon_number(sphingoid_base)
  bond_number <- get_bond_number(sphingoid_base)
  
  # get modifications
  peroxy_number <- get_peroxy_number(sphingoid_base)
  hydroxy_number <- get_hydroxy_number(sphingoid_base)
  keto_number <- get_keto_number(sphingoid_base)
  amino_number <- get_amino_number(sphingoid_base)
  
  # calculate number of atoms (other than C)
  c_count <- carbon_number
  h_count <- 2 * carbon_number - 2 * bond_number - 2 * keto_number + amino_number + 2
  o_count <- hydroxy_number + keto_number + 2 * peroxy_number
  n_count <- amino_number
  
  # calculate sphingoid formula
  sphingoid_formula <- paste0("C", c_count,
                              "H", h_count,
                              "O", o_count,
                              "N", n_count)
  
  sphingoid_formula <- MetaboCoreUtils::standardizeFormula(sphingoid_formula)
  
  # return sphingoid formula
  return(sphingoid_formula)
}