#' @title Generation of different shorthand notation level
#' 
#' This function simplifies a given shorthand notation to a lower level notation, e.g. CoA(18:2(9Z,12Z)) to CoA(18:2). Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#' 
#' @param x Shorthand notation of a fatty acyl lipid, e.g. CoA(18:2(9Z,12Z))
#' @param level Either \code{structural}, \code{molecular} or \code{species} for the respective levels according to SwissLipids hierachy
#' @examples 
#' library(lipidomicsUtils)
#' get_fa_shorthand("CoA(18:2(9Z,12Z))")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{get_fa_biggid}}
#'
#' @export
get_fa_shorthand <- function(x, level = c("structural", "molecular", "species")) {
  
  # detection of functional groups
  if(stringr::str_detect(x, "\\d+(OH|OOH|O|NH2)")) {
    stop("Functional groups other than methyl branches on fatty acyls currently not supported, only DBs")
  }
  
  # all levels are the same like the species level (only one acyl!)
  if(level == "structural") {
    
    lipid <- .get_fa_species_level(x)
    
  } else if(level == "molecular") {
    
    lipid <- .get_fa_species_level(x)
    
  } else if(level == "species") {
    
    lipid <- .get_fa_species_level(x)
    
  }
  
  # return result
  return(lipid)
  
}

#' @title Generation of a BiGG like ID for an acyl based shorthand notation
#' 
#' This function generates a BiGG like ID for GSMNs for a given shorthand notation, e.g. CoA(18:2(9Z,12Z)) to coa18_2_9z12z. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#' 
#' @param x Shorthand notation of a fatty acyl lipid, e.g. CoA(18:2(9Z,12Z))
#' @examples 
#' library(lipidomicsUtils)
#' get_fa_biggid("CoA(18:2(9Z,12Z))")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{get_fa_shorthand}}
#'
#' @export
get_fa_biggid <- function(x) {
  
  # basic information
  lipid_main_class <- lipidomicsUtils::get_lipid_mainclass(x)
  carbon_number <- lipidomicsUtils::get_carbon_number(x)
  bond_number<- lipidomicsUtils::get_bond_number(x)
  
  # get functional groups
  double_bonds <- lipidomicsUtils::get_double_bonds(x)
  hydroxy_groups <- lipidomicsUtils::get_hydroxy_groups(x)
  peroxy_groups <- lipidomicsUtils::get_peroxy_groups(x)
  keto_groups <- lipidomicsUtils::get_keto_groups(x)
  amino_groups <- lipidomicsUtils::get_amino_groups(x)
  methyl_branches <- lipidomicsUtils::get_methyl_branches(x)
  
  # remove potential stereo
  hydroxy_groups <- unlist(stringr::str_remove_all(hydroxy_groups, "\\[(S|R)\\]"))
  peroxy_groups <- unlist(stringr::str_remove_all(peroxy_groups, "\\[(S|R)\\]"))
  amino_groups <- unlist(stringr::str_remove_all(amino_groups, "\\[(S|R)\\]"))
  methyl_branches <- unlist(stringr::str_remove_all(methyl_branches, "\\[(S|R)\\]"))
  
  # get stereocenters
  stereo <- unlist(stringr::str_extract_all(x, "\\[(S|R)\\]"))
  
  # create BiGG ID
  bigg_id <- paste0(stringr::str_to_lower(lipid_main_class),
                    carbon_number, "_",
                    bond_number, "_")
  
  # add different functional groups
  # DB > OH > OOH > O > NH2 > Me
  if(length(double_bonds) > 0) {
    bigg_id <- paste0(bigg_id,
                      paste0(stringr::str_to_lower(double_bonds), collapse = "")
                      )
  }
  
  if(length(hydroxy_groups) > 0) {
    bigg_id <- paste0(bigg_id,
                      paste0(stringr::str_to_lower(hydroxy_groups), collapse = ""))
  }
  
  if(length(peroxy_groups) > 0) {
    bigg_id <- paste0(bigg_id,
                      paste0(stringr::str_to_lower(peroxy_groups), collapse = ""))
  }
  
  if(length(keto_groups) > 0) {
    bigg_id <- paste0(bigg_id,
                      paste0(stringr::str_to_lower(keto_groups), collapse = ""))
  }
  
  if(length(amino_groups) > 0) {
    bigg_id <- paste0(bigg_id,
                      paste0(stringr::str_to_lower(amino_groups), collapse = ""))
  }
  
  if(length(methyl_branches) > 0) {
    bigg_id <- paste0(bigg_id,
                      paste0(stringr::str_to_lower(methyl_branches), collapse = ""))
  }
  
  # add stereo chemistry
  if(length(stereo) > 0) {
    bigg_id <- paste0(bigg_id, "__",
                      paste0(stringr::str_remove_all(stereo, "\\[|\\]"), collapse = ""))
  }
  
  # remove potential ending "_"
  bigg_id <- stringr::str_replace(bigg_id, "_$", "")
  
  # return BiGG ID
  return(bigg_id)
}

#' Private function to generate species level shorthand
#'
#'
.get_fa_species_level <- function(x) {
  
  # get subclass
  lipid_subclass <- lipidomicsUtils::get_lipid_subclass(x)
  lipid_mainclass <- lipidomicsUtils::get_lipid_mainclass(x)
  
  # isolate fatty acids
  fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(x)
  
  # get total carbon and double bond number
  total_carbon <- sum(unlist(lapply(fatty_acyls, function(x) {
    lipidomicsUtils::get_carbon_number(x)
  })))
  
  total_bond <- sum(unlist(lapply(fatty_acyls, function(x) {
    lipidomicsUtils::get_bond_number(x)
  })))
  
  species <- paste0(lipid_mainclass, "(", total_carbon, ":", total_bond, ")")
  
  # return result
  return(species)
  
}