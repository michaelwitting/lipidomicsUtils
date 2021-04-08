#' @title Get number of carbons
#' 
#' This function returns the number of carbons in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_carbon_number("d16:1(1OH,3OH,15Me)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_bond_number}}
#' @seealso \code{\link{get_hydroxy_number}}
#' @seealso \code{\link{get_peroxy_number}}
#' @seealso \code{\link{get_keto_number}}
#' @seealso \code{\link{get_amino_number}}
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

#' @title Get number of Double Bonds
#' 
#' This function returns the number of double bonds in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_bond_number("d16:1(1OH,3OH,15Me)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_carbon_number}}
#' @seealso \code{\link{get_hydroxy_number}}
#' @seealso \code{\link{get_peroxy_number}}
#' @seealso \code{\link{get_keto_number}}
#' @seealso \code{\link{get_amino_number}}
#'
#' @export
get_bond_number <- function(x) {
  
  base_carbon <- stringr::str_extract(x, "\\d+:\\d+")[[1]][1]
  
  bond_number <-
    as.numeric(stringr::str_split(base_carbon, ":")[[1]][2])
  
  if(length(get_ring_groups(x)) > 0) {
    
    rings <- get_ring_groups(x)
    ringdbs <- 0
    
    for(ring in rings) {
      
      print(ring)
      
      ringdbs <- ringdbs + 1 + as.numeric(stringr::str_replace(stringr::str_split(ring, ":")[[1]][2], "\\(\\d+\\)", ""))
    }
    
    bond_number <- bond_number + ringdbs
    
  }
  
  bond_number
  
}

#' @title Get number of hydroxy groups
#' 
#' This function returns the number of hydroxy groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_hydroxy_number("d16:1(1OH,3OH,15Me)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_carbon_number}}
#' @seealso \code{\link{get_bond_number}}
#' @seealso \code{\link{get_peroxy_number}}
#' @seealso \code{\link{get_keto_number}}
#' @seealso \code{\link{get_amino_number}}
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

#' @title Get number of peroxy groups
#' 
#' This function returns the number of peroxy groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_peroxy_number("18:0(5OOH[R])")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_carbon_number}}
#' @seealso \code{\link{get_bond_number}}
#' @seealso \code{\link{get_hydroxy_number}}
#' @seealso \code{\link{get_keto_number}}
#' @seealso \code{\link{get_amino_number}}
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

#' @title Get number of keto groups
#' 
#' This function returns the number of keto groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_keto_number("18:0(3O)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_carbon_number}}
#' @seealso \code{\link{get_bond_number}}
#' @seealso \code{\link{get_hydroxy_number}}
#' @seealso \code{\link{get_peroxy_number}}
#' @seealso \code{\link{get_amino_number}}
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

#' @title Get number of keto groups
#' 
#' This function returns the number of keto groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_amino_number("18:0(2NH2)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_carbon_number}}
#' @seealso \code{\link{get_bond_number}}
#' @seealso \code{\link{get_hydroxy_number}}
#' @seealso \code{\link{get_peroxy_number}}
#' @seealso \code{\link{get_keto_number}}
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

#' @title Get all methyl branches
#' 
#' This function returns all methyl branches in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_methyl_branches("d16:1(1OH,3OH,15Me)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_double_bonds}}
#' @seealso \code{\link{get_hydroxy_groups}}
#' @seealso \code{\link{get_keto_groups}}
#' @seealso \code{\link{get_peroxy_groups}}
#' @seealso \code{\link{get_amino_groups}}
#'
#' @export
get_methyl_branches <- function(x) {
  
  # get all methyl branches in lipids
  methyl_branches <- unlist(stringr::str_extract_all(x, "\\d+Me")[[1]])
  
  return(methyl_branches)
  
}

#' @title Get all double bonds
#' 
#' This function returns all double bonds in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_double_bonds("d16:1(1OH,3OH,15Me)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_methyl_branches}}
#' @seealso \code{\link{get_hydroxy_groups}}
#' @seealso \code{\link{get_keto_groups}}
#' @seealso \code{\link{get_peroxy_groups}}
#' @seealso \code{\link{get_amino_groups}}
#'
#' @export
get_double_bonds <- function(x) {
  
  # get all double bonds in lipids
  unlist(stringr::str_extract_all(x, "\\d+(Z|E)")[[1]])
  
}

#' @title Get all hydroxy groups
#' 
#' This function returns all hydroxy groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_double_bonds("d16:1(1OH,3OH,15Me)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_methyl_branches}}
#' @seealso \code{\link{get_double_bonds}}
#' @seealso \code{\link{get_keto_groups}}
#' @seealso \code{\link{get_peroxy_groups}}
#' @seealso \code{\link{get_amino_groups}}
#'
#' @export
get_hydroxy_groups <- function(x) {
  
  # get all hydroxy groups in lipids
  unlist(stringr::str_extract_all(x, "\\d+(OH)(\\[(S|R)\\])*")[[1]])
  
}

#' @title Get all keto groups
#' 
#' This function returns all keto groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_keto_groups("18:0(3O)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_methyl_branches}}
#' @seealso \code{\link{get_double_bonds}}
#' @seealso \code{\link{get_hydroxy_groups}}
#' @seealso \code{\link{get_peroxy_groups}}
#' @seealso \code{\link{get_amino_groups}}
#'
#' @export
get_keto_groups <- function(x) {
  
  # get all "keto" groups in lipids
  keto_groups <- unlist(stringr::str_extract_all(x, "\\d+O")[[1]])
  
  # get all hydroxy groups for filterin
  hydroxy_groups <- unlist(stringr::str_extract_all(x, "\\d+(OH)(\\[(S|R)\\])*")[[1]])
  hydroxy_groups <- unlist(stringr::str_remove_all(hydroxy_groups, "\\[(S|R)\\]"))
  hydroxy_groups <- unlist(stringr::str_remove_all(hydroxy_groups, "H"))
  
  # filter keto groups to be not present in hydroxyl groups
  keto_groups <- keto_groups[!keto_groups %in% hydroxy_groups]
  
  # get all peroxygroups
  peroxy_groups <- unlist(stringr::str_extract_all(x, "\\d+(OOH)(\\[(S|R)\\])*")[[1]])
  peroxy_groups <- unlist(stringr::str_remove_all(peroxy_groups, "\\[(S|R)\\]"))
  peroxy_groups <- unlist(stringr::str_remove_all(peroxy_groups, "OH"))
  
  keto_groups <- keto_groups[!keto_groups %in% peroxy_groups]

  if(length(keto_groups) == 0) {
    keto_groups <- NA
  }
  
  keto_groups
}

#' @title Get all peroxy groups
#' 
#' This function returns all peroxy groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_peroxy_groups("18:0(5OOH)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_methyl_branches}}
#' @seealso \code{\link{get_double_bonds}}
#' @seealso \code{\link{get_hydroxy_groups}}
#' @seealso \code{\link{get_keto_groups}}
#' @seealso \code{\link{get_amino_groups}}
#'
#' @export
get_peroxy_groups <- function(x) {
  
  # get all peroxy gorups in lipids
  unlist(stringr::str_extract_all(x, "\\d+OOH"))
  
}

#' @title Get all amino groups
#' 
#' This function returns all amino groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_peroxy_groups("18:0(5NH2)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_methyl_branches}}
#' @seealso \code{\link{get_double_bonds}}
#' @seealso \code{\link{get_hydroxy_groups}}
#' @seealso \code{\link{get_keto_groups}}
#' @seealso \code{\link{get_peroxy_groups}}
#'
#' @export
get_amino_groups <- function(x) {
  
  # get all amino groups in lipids
  unlist(stringr::str_extract_all(x, "\\d+NH2"))
  
}

#' @title Get all rings in an acyl
#' 
#' This function returns all amino groups in a acyl, alkyl, alkenyl, sphingoid base etc.
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' get_peroxy_groups("18:0(5NH2)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_methyl_branches}}
#' @seealso \code{\link{get_double_bonds}}
#' @seealso \code{\link{get_hydroxy_groups}}
#' @seealso \code{\link{get_keto_groups}}
#' @seealso \code{\link{get_peroxy_groups}}
#'
#' @export
get_ring_groups <- function(x) {
  
  # get all amino groups in lipids
  unlist(stringr::str_extract_all(x, "\\d+-\\d+cy\\d+:\\d+(\\(\\d+\\))*"))

}