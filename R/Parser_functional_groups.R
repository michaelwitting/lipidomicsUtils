#' @title Get number of carbons
#' 
#' This function returns the number of carbons in a acyl, alkyl, alkenyl or sphingoid base based on a supplied shorthand notation, e.g. d16:1(4E)(1OH,3OH)(15Me)
#' 
#' @param x Shorthand notation of a acyl, alkyl, alkenyl (as string), e.g. "d16:1(4E)(1OH,3OH)(15Me)"
#' @example 
#' get_carbon_number("d16:1(4E)(1OH,3OH)(15Me)")
#'
#' @export
get_carbon_number <- function(x) {
  
  base_carbon <- stringr::str_extract(x, "\\d+:\\d+")[[1]][1]
  methyl_branches <- stringr::str_count(x, "Me")
  
  carbon_number <-
    as.numeric(stringr::str_split(base_carbon, ":")[[1]][1]) +
    methyl_branches
  
  return(carbon_number)
  
}

#' @title Get number of double bond
#'
#' @export
get_bond_number <- function(x) {
  
  base_carbon <- stringr::str_extract(x, "\\d+:\\d+")[[1]][1]
  
  bond_number <-
    as.numeric(stringr::str_split(base_carbon, ":")[[1]][2])
  
  return(bond_number)
  
}

#'
#'
#' @export
get_hydroxy_number <- function(x) {
  
  # check if sphingoid base
  if(stringr::str_detect(x, "(m|d|t)")) {
    
    hydroxy_shorthand <- stringr::str_extract(x, "(m|d|t)")
    
    if(hydroxy_shorthand == "m") {
      
      hydroxy_number <- 1
      
    } else if(hydroxy_shorthand == "d") {
      
      hydroxy_number <- 2
      
    } else if(hydroxy_shorthand == "m") {
      
      hydroxy_number <- 3
      
    }
    
  } else {
    
    # get number of individual groups
    peroxy_number <- stringr::str_count(x, "(?:O)(OH)")[[1]]
    hydroxy_number <- stringr::str_count(x, "OH")[[1]] - peroxy_number
    keto_number <- stringr::str_count(x, "O") - 2 * peroxy_number - hydroxy_number
    
    if(stringr::str_detect(x, "O-")) {
      keto_number <- keto_number - 1
    }
    
  }

  return(hydroxy_number)
  
}

#'
#'
#' @export
get_peroxy_number <- function(x) {
  
  # get number of individual groups
  peroxy_number <- stringr::str_count(x, "(?:O)(OH)")[[1]]
  hydroxy_number <- stringr::str_count(x, "OH")[[1]] - peroxy_number
  keto_number <- stringr::str_count(x, "O") - 2 * peroxy_number - hydroxy_number
  
  if(stringr::str_detect(x, "O-")) {
    keto_number <- keto_number - 1
  }
  
  return(peroxy_number)
  
}

#'
#'
#' @export
get_keto_number <- function(x) {
  
  # get number of individual groups
  peroxy_number <- stringr::str_count(x, "(?:O)(OH)")[[1]]
  hydroxy_number <- stringr::str_count(x, "OH")[[1]] - peroxy_number
  keto_number <- stringr::str_count(x, "O") - 2 * peroxy_number - hydroxy_number
  
  if(stringr::str_detect(x, "O-")) {
    keto_number <- keto_number - 1
  }
  
  return(keto_number)
  
}

#'
#'
#' @export
get_amino_number <- function(x) {
  
  # check if sphingoid base
  if(stringr::str_detect(x, "(m|d|t)")) {
    
    amino_number <- 1
    
  } else {
    
    # get number of individual groups
    amino_number <- stringr::str_count(x, "NH2")[[1]]
    
  }
  
  return(amino_number)
  
}