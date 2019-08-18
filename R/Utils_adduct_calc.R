#' @title Calculate adduct mass from neutral exact mass
#' 
#' This function calculates the m/z from a given exact mass and a valid adduct defintion, e.g. [M+H]+
#' 
#' @param exact_mass neutral exact mass
#' @param adduct adduct definition, e.g. [M+H]+
#' @examples 
#' library(lipidomicsUtils)
#' calc_adduct_mass(731.5465, "[M+H]+")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{calc_neutral_mass}}
#' @seealso \code{\link{create_ion_formula}}
#'
#' @export
calc_adduct_mass <- function(exact_mass, adduct) {
  
  ion_mass <- metabolomicsUtils::calc_adduct_mass(exact_mass, adduct)
  
  return(ion_mass)
  
}

#' @title Calculate neutral exact mass from adduct m/z
#' 
#' This function calculates neutral exact mass from a given m/z and a valid adduct defintion, e.g. [M+H]+
#' 
#' @param exact_mass neutral exact mass
#' @param adduct adduct definition, e.g. [M+H]+
#' @examples 
#' library(lipidomicsUtils)
#' calc_neutral_mass(732.5538, "[M+H]+")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{calc_adduct_mass}}
#' @seealso \code{\link{create_ion_formula}}
#'
#' @export
calc_neutral_mass <- function(ion_mass, adduct) {
  
  exact_mass <- metabolomicsUtils::calc_neutral_mass(ion_mass, adduct)
  
  return(exact_mass)
  
}

#' @title Create ion formula from neutral formula and adduct
#' 
#' This function creates an ion formula from a neutral chemical formula and a valid adduct defintion, e.g. [M+H]+
#' 
#' @param exact_mass neutral exact mass
#' @param adduct adduct definition, e.g. [M+H]+
#' @examples 
#' library(lipidomicsUtils)
#' create_ion_formula("C6H12O6", "[M+H]+")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{calc_adduct_mass}}
#' @seealso \code{\link{calc_neutral_mass}}
#'
#' @export
create_ion_formula <- function(chem_formula, adduct) {

  ion_formula <- metabolomicsUtils::create_ion_formula(chem_formula, adduct)

  # return result
  return(ion_formula)
}
