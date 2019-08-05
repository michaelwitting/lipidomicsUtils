#'
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