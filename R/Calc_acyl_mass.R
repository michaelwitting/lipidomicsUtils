#' @title Calculation of acyl, alkyl or alkenyl mass (intact fatty acid or fatty alcohol)
#' 
#' This function calculates the mass of an acyl, alkyl or alkenyl given as shorthand notation, e.g. "18:1(9Z)" and calculates the mass of the respective intact fatty acid or alcohol. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#' 
#' @param x Shorthand notation of a acyl, alkyl, alkenyl (as string), e.g. "18:1(9Z)"
#' @example 
#' calc_acyl_mass("18:1(9Z)")
#'
#' @export
calc_intact_acyl_mass <- function(x) {
  
  # check if acyl, alkyl or alkenyl
  if(stringr::str_detect(x, "O-")) {
    intact_mass <- .alkyl_mass(x)
  } else if(stringr::str_detect(x, "P-")) {
    intact_mass <- .alkenyl_mass(x)
  } else {
    intact_mass <- .acyl_mass(x)
  }

  # return fatty acid mass
  return(intact_mass)
}

#' @title Calculation of acyl, alkyl or alkenyl mass (residue fatty acid or fatty alcohol)
#'
#' This function calculates the mass of an acyl, alkyl or alkenyl given as shorthand notation, e.g. "18:1(9Z)" and calculates the mass of the respective residue fatty acid or alcohol. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#'
#' @param x Shorthand notation of a acyl, alkyl, alkenyl (as string), e.g. "18:1(9Z)"
#' @example
#' calc_acyl_mass("18:1(9Z)")
#'
#' @export
calc_residue_acyl_mass <- function(x) {

  # check if acyl, alkyl or alkenyl
  if(stringr::str_detect(x, "O-")) {
    intact_mass <- .alkyl_mass(x)
  } else if(stringr::str_detect(x, "P-")) {
    intact_mass <- .alkenyl_mass(x)
  } else {
    intact_mass <- .acyl_mass(x)
  }
  
  residue_mass <- intact_mass - lipidomicsUtils:::o_mass - 2 * lipidomicsUtils:::h_mass

  # return residue mass
  return(residue_mass)
}

#' Private function for calculation of intact acyl mass
#'
#'
.acyl_mass <- function(x) {
  
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
  
  # calculate fatty acid mass
  fatty_acid_mass <- c_count * lipidomicsUtils:::c_mass +
    h_count * lipidomicsUtils:::h_mass +
    o_count * lipidomicsUtils:::o_mass +
    n_count * lipidomicsUtils:::n_mass
  
  # return fatty acid mass
  return(fatty_acid_mass)
  
}

#' Private function for calculation of intact alkyl mass
#'
#'
.alkyl_mass <- function(x) {
  
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

  # calculate fatty acid mass
  fatty_alkyl_mass <- c_count * lipidomicsUtils:::c_mass +
    h_count * lipidomicsUtils:::h_mass +
    o_count * lipidomicsUtils:::o_mass +
    n_count * lipidomicsUtils:::n_mass
  
  # return fatty acid mass
  return(fatty_alkyl_mass)
  
}

#' Private function for calculation of intact alkenyl mass
#'
#'
.alkenyl_mass <- function(x) {
  
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
  
  # calculate fatty acid mass
  fatty_alkenyl_mass <- c_count * lipidomicsUtils:::c_mass +
    h_count * lipidomicsUtils:::h_mass +
    o_count * lipidomicsUtils:::o_mass +
    n_count * lipidomicsUtils:::n_mass
  
  # return fatty acid mass
  return(fatty_alkenyl_mass)
  
}