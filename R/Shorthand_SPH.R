#'
#'
#' @export
get_sph_shorthand <- function(x, level = c("structural", "molecular", "species")) {
  
  # detection of functional groups
  # if(stringr::str_detect(x, "\\d+(OOH|O|NH2)")) {
  #   stop("Functional groups other than hydroxyl groups & methyl branches on fatty acyls currently not supported, only DBs")
  # }
  
  if(level == "structural") {
    
    lipid <- .get_sph_position_level(x)
    
  } else if(level == "molecular") {
    
    # no difference to structural level!!!
    lipid <- .get_sph_position_level(x)
    
  } else if(level == "species") {
    
    lipid <- .get_sph_species_level(x)
    
  }
  
  # return result
  return(lipid)
  
}

.get_sph_position_level <- function(x) {
  
  # get subclass
  lipid_subclass <- lipidomicsUtils::get_lipid_subclass(x)
  lipid_class <- lipidomicsUtils::get_lipid_class(x)
  
  # isolate sphingoid base and fatty acyl
  sphingoid <- lipidomicsUtils::isolate_sphingoid_base(x)
  fatty_acyl <- lipidomicsUtils::isolate_fatty_acyls(x)
  
  # get building plocks
  sphingoid_carbon <- lipidomicsUtils::get_carbon_number(sphingoid)
  sphingoid_bonds <- lipidomicsUtils::get_bond_number(sphingoid)
  sphingoid_hydroxy <- lipidomicsUtils::get_hydroxy_number(sphingoid)
  
  fatty_acyl_carbon <- lipidomicsUtils::get_carbon_number(fatty_acyl)
  fatty_acyl_bonds <- lipidomicsUtils::get_bond_number(fatty_acyl)
  fatty_acyl_hydroxy <- lipidomicsUtils::get_hydroxy_number(fatty_acyl)
  
  # make simple version
  # correct lipid class
  if(lipid_class %in% c("GlcCer", "GalCer")) {
    
    lipid_class <- "HexCer"
    
  } else if(lipid_class %in% c("LacCer")) {
    
    lipid_class <- "Hex2Cer"
    
  }
  
  # number of hydroxy in sphingoid base
  if(sphingoid_hydroxy == 1) {
    hydroxy <- "m"
  } else if(sphingoid_hydroxy == 2) {
    hydroxy <- "d"
  } else if(sphingoid_hydroxy == 3) {
    hydroxy <- "t"
  }
  
  # make lipid
  position <- paste0(lipid_class, "(", hydroxy, sphingoid_carbon, ":", sphingoid_bonds, "/", fatty_acyl_carbon, ":", fatty_acyl_bonds)
  
  if(fatty_acyl_hydroxy == 1) {
    position <- paste0(position, "(OH))")
  } else if(fatty_acyl_hydroxy > 1) {
    position <- paste0(position, "(OH)", fatty_acyl_hydroxy, ")")
  } else {
    position <- paste0(position, ")")
  }
  
  # return lipid
  return(position)
  
}

.get_sph_species_level <- function(x) {
  
  # get subclass
  lipid_subclass <- lipidomicsUtils::get_lipid_subclass(x)
  lipid_class <- lipidomicsUtils::get_lipid_class(x)
  
  # isolate sphingoid base and fatty acyl
  sphingoid <- lipidomicsUtils::isolate_sphingoid_base(x)
  fatty_acyl <- lipidomicsUtils::isolate_fatty_acyls(x)
  
  # get building plocks
  sphingoid_carbon <- lipidomicsUtils::get_carbon_number(sphingoid)
  sphingoid_bonds <- lipidomicsUtils::get_bond_number(sphingoid)
  sphingoid_hydroxy <- lipidomicsUtils::get_hydroxy_number(sphingoid)
  
  fatty_acyl_carbon <- lipidomicsUtils::get_carbon_number(fatty_acyl)
  fatty_acyl_bonds <- lipidomicsUtils::get_bond_number(fatty_acyl)
  fatty_acyl_hydroxy <- lipidomicsUtils::get_hydroxy_number(fatty_acyl)
  
  # make simple version
  # correct lipid class
  if(lipid_class %in% c("GlcCer", "GalCer")) {
    
    lipid_class <- "HexCer"
    
  } else if(lipid_class %in% c("LacCer")) {
    
    lipid_class <- "Hex2Cer"
    
  }
  
  # number of hydroxy in sphingoid base
  if(sphingoid_hydroxy + fatty_acyl_hydroxy == 1) {
    hydroxy <- "m"
  } else if(sphingoid_hydroxy + fatty_acyl_hydroxy == 2) {
    hydroxy <- "d"
  } else if(sphingoid_hydroxy + fatty_acyl_hydroxy == 3) {
    hydroxy <- "t"
  } else if(sphingoid_hydroxy + fatty_acyl_hydroxy == 4) {
    hydroxy <- "q"
  }
  
  # get total carbon and bonds
  carbon_total <- sphingoid_carbon + fatty_acyl_carbon
  bonds_total <- sphingoid_bonds + fatty_acyl_bonds
  
  # create lipid
  species <- paste0(lipid_class, "(", hydroxy, carbon_total, ":", bonds_total, ")")
  
  return(species)
  
}
