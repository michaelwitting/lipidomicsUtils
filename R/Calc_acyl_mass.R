#' @title Calculation of acyl, alkyl or alkenyl mass (intact fatty acid or fatty alcohol)
#' 
#' This function calculates the mass of an acyl, alkyl or alkenyl given as shorthand notation, e.g. "18:1(9Z)" and calculates the mass of the respective intact fatty acid or alcohol. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#' 
#' @param x Shorthand notation of a acyl, alkyl, alkenyl (as string), e.g. "18:1(9Z)"
#' @examples 
#' library(lipidomicsUtils)
#' calc_intact_acyl_mass("18:1(9Z)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{calc_residue_acyl_mass}}
#' @seealso \code{\link{calc_intact_acyl_formula}}
#' @seealso \code{\link{calc_residue_acyl_formula}}
#'
#' @export
calc_intact_acyl_mass <- function(x) {
  
  if(x == "0:0") {
    return(0.0)
  }
  
  # check if acyl, alkyl or alkenyl
  if(stringr::str_detect(x, "O-")) {
    
    intact_mass <- rcdk::get.formula(.alkyl_formula(x))@mass
    
  } else if(stringr::str_detect(x, "P-")) {
    
    intact_mass <-  rcdk::get.formula(.alkenyl_formula(x))@mass
    
  } else {
    
    intact_mass <- rcdk::get.formula(.acyl_formula(x))@mass
    
  }

  # return fatty acid mass
  return(intact_mass)
}

#' @title Calculation of acyl, alkyl or alkenyl mass (residue fatty acid or fatty alcohol)
#'
#' This function calculates the mass of an acyl, alkyl or alkenyl given as shorthand notation, e.g. "18:1(9Z)" and calculates the mass of the respective residue fatty acid or alcohol. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#'
#' @param x Shorthand notation of a acyl, alkyl, alkenyl (as string), e.g. "18:1(9Z)"
#' @examples 
#' library(lipidomicsUtils)
#' calc_residue_acyl_mass("18:1(9Z)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{calc_intact_acyl_mass}}
#' @seealso \code{\link{calc_intact_acyl_formula}}
#' @seealso \code{\link{calc_residue_acyl_formula}}
#'
#' @export
calc_residue_acyl_mass <- function(x) {
  
  if(x == "0:0") {
    return(0.0)
  }

  # check if acyl, alkyl or alkenyl
  if(stringr::str_detect(x, "O-")) {
    
    intact_mass <- rcdk::get.formula(.alkyl_formula(x))@mass
    
  } else if(stringr::str_detect(x, "P-")) {
    
    intact_mass <-  rcdk::get.formula(.alkenyl_formula(x))@mass
    
  } else {
    
    intact_mass <- rcdk::get.formula(.acyl_formula(x))@mass
    
  }
  
  residue_mass <- intact_mass - rcdk::get.formula("H2O")@mass

  # return residue mass
  return(residue_mass)
}

#' @title Calculation of acyl, alkyl or alkenyl formula (intact fatty acid or fatty alcohol)
#'
#' This function calculates the formula of an acyl, alkyl or alkenyl given as shorthand notation, e.g. "18:1(9Z)" and calculates the formula of the respective intact fatty acid or alcohol. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#'
#' @param x Shorthand notation of a acyl, alkyl, alkenyl (as string), e.g. "18:1(9Z)"
#' @examples 
#' library(lipidomicsUtils)
#' calc_intact_acyl_formula("18:1(9Z)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{calc_intact_acyl_mass}}
#' @seealso \code{\link{calc_intact_acyl_mass}}
#' @seealso \code{\link{calc_residue_acyl_formula}}
#'
#' @export
calc_intact_acyl_formula <- function(x) {
  
  if(x == "0:0") {
    return("C0")
  }
  
  # check if acyl, alkyl or alkenyl
  if(stringr::str_detect(x, "O-")) {
    
    intact_formula <- .alkyl_formula(x)
    
  } else if(stringr::str_detect(x, "P-")) {
    
    intact_formula <- .alkenyl_formula(x)
    
  } else {
    
    intact_formula <- .acyl_formula(x)
    
  }
  
  # return formula
  return(intact_formula)
  
}

#' @title Calculation of acyl, alkyl or alkenyl formula (residue fatty acid or fatty alcohol)
#'
#' This function calculates the formula of an acyl, alkyl or alkenyl given as shorthand notation, e.g. "18:1(9Z)" and calculates the formula of the respective residue fatty acid or alcohol. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#'
#' @param x Shorthand notation of a acyl, alkyl, alkenyl (as string), e.g. "18:1(9Z)"
#' @examples 
#' library(lipidomicsUtils)
#' calc_residue_acyl_formula("18:1(9Z)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{calc_intact_acyl_mass}}
#' @seealso \code{\link{calc_intact_acyl_mass}}
#' @seealso \code{\link{calc_intact_acyl_formula}}
#'
#' @export
calc_residue_acyl_formula <- function(x) {
  
  if(x == "0:0") {
    return("C0")
  }
  
  # check if acyl, alkyl or alkenyl
  if(stringr::str_detect(x, "O-")) {
    
    intact_formula <- .alkyl_formula(x)
    
  } else if(stringr::str_detect(x, "P-")) {
    
    intact_formula <- .alkenyl_formula(x)
    
  } else {
    
    intact_formula <- .acyl_formula(x)
    
  }
  
  residue_formula <- MetaboCoreUtils::subtractElements(intact_formula, "H2O")
  
  # return formula
  return(residue_formula)
  
}

#' Private function for calculation of intact acyl formula
#'
#'
.acyl_formula <- function(x) {
  
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
  h_count <- 2 * carbon_number - 2 * bond_number - 2 * keto_number + amino_number
  o_count <- 2 + hydroxy_number + keto_number + 2 * peroxy_number
  n_count <- amino_number
  
  # calculate formula
  acyl_formula <- paste0("C", c_count,
                         "H", h_count,
                         "O", o_count,
                         "N", n_count)
    
  acyl_formula <- MetaboCoreUtils::standardizeFormula(acyl_formula)
  
  # return formula
  return(acyl_formula)
  
}

#' Private function for calculation of intact alkyl formula
#'
#'
.alkyl_formula <- function(x) {
  
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
  o_count <- 1 + hydroxy_number + keto_number + 2 * peroxy_number
  n_count <- amino_number

  # calculate formula
  alkyl_formula <- paste0("C", c_count,
                          "H", h_count,
                          "O", o_count,
                          "N", n_count)
  
  alkyl_formula <- MetaboCoreUtils::standardizeFormula(alkyl_formula)
  
  # return formula
  return(alkyl_formula)
  
}

#' Private function for calculation of intact alkenyl formula
#'
#'
.alkenyl_formula <- function(x) {
  
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
  h_count <- 2 * carbon_number - 2 * bond_number - 2 * keto_number + amino_number + 2 - 2
  o_count <- 1 + hydroxy_number + keto_number + 2 * peroxy_number
  n_count <- amino_number
  
  # calculate formula
  alkenyl_formula <- paste0("C", c_count,
                          "H", h_count,
                          "O", o_count,
                          "N", n_count)
  
  alkenyl_formula <- MetaboCoreUtils::standardizeFormula(alkenyl_formula)
  
  # return formula
  return(alkenyl_formula)
  
}